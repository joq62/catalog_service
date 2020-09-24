%% This is the application resource file (.app file) for the 'base'
%% application.
{application, catalog_service,
[{description, "catalog_service" },
{vsn, "0.0.1" },
{modules, 
	  [catalog_service_app,catalog_service_sup,catalog_service,
		catalog]},
{registered,[catalog_service]},
{applications, [kernel,stdlib]},
{mod, {catalog_service_app,[]}},
{start_phases, []}
]}.
