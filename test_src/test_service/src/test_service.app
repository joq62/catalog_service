%% This is the application resource file (.app file) for the 'base'
%% application.
{application, test_service,
[{description, "test agent" },
{vsn, "1.0.0" },
{modules, 
	  [test_service_app,test_service_sup,test_service]},
{registered,[test_service]},
{applications, [kernel,stdlib]},
{mod, {test_service_app,[]}},
{start_phases, []}
]}.
