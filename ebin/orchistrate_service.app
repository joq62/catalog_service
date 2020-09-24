%% This is the application resource file (.app file) for the 'base'
%% application.
{application, orchistrate_service,
[{description, "orchistrate_service" },
{vsn, "0.0.1" },
{modules, 
	  [orchistrate_service_app,orchistrate_service_sup,orchistrate_service,
		orchistrate]},
{registered,[orchistrate_service]},
{applications, [kernel,stdlib]},
{mod, {orchistrate_service_app,[]}},
{start_phases, []}
]}.
