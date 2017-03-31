/*
 * Copyright © 2015 - 2017 Lightbend, Inc. <http://www.lightbend.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.lightbend.paradox.markdown

import com.lightbend.paradox.tree.Tree
import com.lightbend.paradox.tree.Tree.{ Forest, Location }
import org.pegdown.ast._

/**
 * Create markdown list for table of contents on a page.
 */
class TableOfContents(pages: Boolean = true, headers: Boolean = true, ordered: Boolean = true, maxDepth: Int = 6, autoExpand: Boolean = false, maxExpandDepth: Int = 1) {

  /**
   * Create a TOC bullet list for a Page.
   */
  def markdown(location: Location[Page]): Node = {
    markdown(location.tree.label.base, Some(location), location.tree)
  }

  /**
   * Create a TOC bullet list for a TOC at a certain point within the section hierarchy.
   */
  def markdown(location: Location[Page], tocIndex: Int): Node = {
    markdown(location.tree.label.base, Some(location), nested(location.tree, tocIndex))
  }

  /**
   * Create a TOC bullet list for a Page tree, given the base path and active location.
   */
  def markdown(base: String, active: Option[Location[Page]], tree: Tree[Page]): Node = {
    subList(base, active, tree, depth = 0, expandDepth = None).getOrElse(list(Nil))
  }

  /**
   * Create a TOC bullet list from the root location.
   */
  def root(location: Location[Page]): Node = {
    markdown(location.tree.label.base, Some(location), location.root.tree)
  }

  /**
   * Create a TOC bullet list for the headers of a Page only, including the top-level header.
   */
  def headers(location: Location[Page]): Node = {
    val page = location.tree.label
    val tree = Tree.leaf(page.copy(headers = List(Tree(page.h1, page.headers))))
    markdown(base = page.base, active = None, tree)
  }

  /**
   * Create a new Page Tree for a TOC at a certain point within the section hierarchy.
   */
  private def nested(tree: Tree[Page], tocIndex: Int): Tree[Page] = {
    val page = tree.label
    val (level, headers) = headersBelow(Location.forest(page.headers), tocIndex)
    val subPages = if (level == 0) tree.children else Nil
    Tree(page.copy(headers = headers), subPages)
  }

  /**
   * Find the headers below the buffer index for a toc directive.
   * Return the level of the next header and sub-headers to render.
   */
  private def headersBelow(location: Option[Location[Header]], index: Int): (Int, Forest[Header]) = location match {
    case Some(loc) =>
      if (loc.tree.label.label.getStartIndex > index) (loc.depth, loc.tree :: loc.rights)
      else headersBelow(loc.next, index)
    case None => (0, Nil)
  }

  private def subList[A <: Linkable](base: String, active: Option[Location[Page]], tree: Tree[A], depth: Int, expandDepth: Option[Int]): Option[Node] = {
    tree.label match {
      case page: Page =>
        val subHeaders = if (headers) items(base + page.path, active, page.headers, depth, expandDepth) else Nil
        val subPages = if (pages) items(base, active, tree.children, depth, expandDepth) else Nil
        optList(subHeaders ::: subPages)
      case header: Header =>
        val subHeaders = if (headers) items(base, active, tree.children, depth, expandDepth) else Nil
        optList(subHeaders)
    }
  }

  private def optList(items: List[Node]): Option[Node] = {
    if (items.isEmpty) None else Some(list(items))
  }

  private def list(items: List[Node]): Node = {
    val parent = new SuperNode
    items.foreach(parent.getChildren.add)
    if (ordered) new OrderedListNode(parent)
    else new BulletListNode(parent)
  }

  private def items[A <: Linkable](base: String, active: Option[Location[Page]], forest: Forest[A], depth: Int, expandDepth: Option[Int]): List[Node] = {
    forest map item(base, active, depth + 1, expandDepth.map(_ + 1))
  }

  private def item[A <: Linkable](base: String, active: Option[Location[Page]], depth: Int, expandDepth: Option[Int])(tree: Tree[A]): Node = {
    val linkable = tree.label
    val label = link(base, linkable.path, linkable.label, active)
    val parent = new SuperNode
    parent.getChildren.add(label)
    if ((depth < maxDepth) || expandDepth.exists(_ < maxExpandDepth) || isActiveAncestor(linkable, active) || beginAutoExpand(linkable, active))
      subList(base, active, tree, depth, expandDepth orElse autoExpandDepth(linkable, active)).foreach(parent.getChildren.add)
    new ListItemNode(parent)
  }

  private def isActiveAncestor[A <: Linkable](linkable: A, active: Option[Location[Page]]): Boolean = {
    autoExpand && active.exists(_.path.drop(1).exists(_.tree.label == linkable))
  }

  private def beginAutoExpand[A <: Linkable](linkable: A, active: Option[Location[Page]]): Boolean = {
    autoExpand && active.exists(_.tree.label == linkable) && (maxExpandDepth > 0)
  }

  private def autoExpandDepth[A <: Linkable](linkable: A, active: Option[Location[Page]]): Option[Int] = {
    if (beginAutoExpand(linkable, active)) Some(0) /* start auto-expanding */ else None
  }

  private def link(base: String, path: String, label: Node, active: Option[Location[Page]]): Node = {
    if (active.exists(_.tree.label.path == path)) new ActiveLinkNode(base + path, label)
    else new ExpLinkNode("", base + path, label)
  }

}
