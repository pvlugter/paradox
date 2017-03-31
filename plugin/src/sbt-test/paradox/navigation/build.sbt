lazy val docs = project
  .in(file("."))
  .enablePlugins(ParadoxPlugin)
  .settings(
    name := "Paradox Navigation Test",
    paradoxTheme := None,
    paradoxNavigationDepth := 1,
    paradoxNavigationExpandActive := true,
    paradoxNavigationExpandDepth := 1
  )
