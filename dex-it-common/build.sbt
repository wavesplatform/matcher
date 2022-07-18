description := "Shared classes for integration tests of DEX and waves-integration"

libraryDependencies ++= Dependencies.Module.dexItCommon

scalacOptions += "-P:silencer:globalFilters=^magnolia: using fallback derivation.*$" // https://github.com/softwaremill/diffx#customization
