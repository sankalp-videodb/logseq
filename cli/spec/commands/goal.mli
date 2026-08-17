type selector = {
  id : Cli_primitive.db_id option;
  uuid : Cli_primitive.uuid option;
}

type create_opts = {
  title : string option;
  description : string option;
  weekly_target : int option;
  weekly_unit : string option;
  daily_check_in : string option;
  task_days : string option;
  reminder_minutes : int option;
}

type check_in_status = Completed | Missed

type parsed =
  | Parsed_list
  | Parsed_show of selector
  | Parsed_create of create_opts
  | Parsed_update of selector * create_opts
  | Parsed_delete of selector
  | Parsed_progress of selector * int option
  | Parsed_check_in of selector * int option * check_in_status option
  | Parsed_pause of selector
  | Parsed_resume of selector
  | Parsed_archive of selector

type action =
  | List of { repo : Cli_primitive.repo; graph : Cli_primitive.graph }
  | Show of {
      repo : Cli_primitive.repo;
      graph : Cli_primitive.graph;
      selector : selector;
    }
  | Create of {
      repo : Cli_primitive.repo;
      graph : Cli_primitive.graph;
      opts : create_opts;
    }
  | Update of {
      repo : Cli_primitive.repo;
      graph : Cli_primitive.graph;
      selector : selector;
      opts : create_opts;
    }
  | Delete of {
      repo : Cli_primitive.repo;
      graph : Cli_primitive.graph;
      selector : selector;
    }
  | Progress of {
      repo : Cli_primitive.repo;
      graph : Cli_primitive.graph;
      selector : selector;
      day : int;
    }
  | Check_in of {
      repo : Cli_primitive.repo;
      graph : Cli_primitive.graph;
      selector : selector;
      day : int;
      status : check_in_status;
    }
  | Set_state of {
      repo : Cli_primitive.repo;
      graph : Cli_primitive.graph;
      selector : selector;
      state : Cli_primitive.keyword;
      record_kind : Cli_primitive.keyword;
    }

include Command_spec.S with type parsed := parsed and type action := action
