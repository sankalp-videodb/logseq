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

let kw = Edn_util.keyword
let vector_vec = Edn_util.vector_vec

let nonempty value =
  let value = String.trim value in
  if value = "" then None else Some value

let today_day () =
  let y, m, d = Time.local_date (Time.now ()) in
  (y * 10000) + (m * 100) + d

let weekday_bit = function
  | "sun" | "sunday" -> Some 1
  | "mon" | "monday" -> Some 2
  | "tue" | "tues" | "tuesday" -> Some 4
  | "wed" | "wednesday" -> Some 8
  | "thu" | "thur" | "thurs" | "thursday" -> Some 16
  | "fri" | "friday" -> Some 32
  | "sat" | "saturday" -> Some 64
  | _ -> None

let task_days_mask value =
  let parts =
    String.split_on_char ',' value
    |> List.map (fun item -> String.lowercase_ascii (String.trim item))
  in
  if parts = [] || List.exists (fun item -> item = "") parts then None
  else
    List.fold_left
      (fun result item ->
        match (result, weekday_bit item) with
        | Some mask, Some bit -> Some (mask lor bit)
        | _ -> None)
      (Some 0) parts

let valid_day day =
  let year = day / 10000 in
  let month = day / 100 mod 100 in
  let date = day mod 100 in
  let leap = year mod 400 = 0 || (year mod 4 = 0 && year mod 100 <> 0) in
  let days_in_month =
    match month with
    | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
    | 4 | 6 | 9 | 11 -> 30
    | 2 -> if leap then 29 else 28
    | _ -> 0
  in
  year >= 1900 && year <= 2999 && date >= 1 && date <= days_in_month

let command_id = function
  | Parsed_list -> Command_id.Goal_list
  | Parsed_show _ -> Goal_show
  | Parsed_create _ -> Goal_create
  | Parsed_update _ -> Goal_update
  | Parsed_delete _ -> Goal_delete
  | Parsed_progress _ -> Goal_progress
  | Parsed_check_in _ -> Goal_check_in
  | Parsed_pause _ -> Goal_pause
  | Parsed_resume _ -> Goal_resume
  | Parsed_archive _ -> Goal_archive

let validate_selector selector =
  match (selector.id, selector.uuid) with
  | Some _, Some _ ->
      Error (Error.invalid_options "only one of --id or --uuid is allowed")
  | None, None ->
      Error (Error.invalid_options "one of --id or --uuid is required")
  | _, Some uuid when not (Cli_primitive.is_uuid_string (String.trim uuid)) ->
      Error (Error.invalid_options "Option uuid must be a valid UUID string")
  | _ -> Ok ()

let validate_parsed = function
  | Parsed_list -> Ok ()
  | Parsed_show selector | Parsed_delete selector -> validate_selector selector
  | Parsed_create opts -> (
      match
        ( Option.bind opts.title nonempty,
          Option.bind opts.daily_check_in nonempty )
      with
      | None, _ -> Error (Error.invalid_options "--title is required")
      | _, None -> Error (Error.invalid_options "--daily-check-in is required")
      | _ -> (
          match (opts.reminder_minutes, opts.task_days) with
          | Some value, _ when value < 0 || value > 1439 ->
              Error
                (Error.invalid_options
                   "--reminder-minutes must be between 0 and 1439")
          | _, Some value when Option.is_none (task_days_mask value) ->
              Error
                (Error.invalid_options
                   "--task-days must be comma-separated weekday names")
          | _ -> Ok ()))
  | Parsed_update (selector, opts) ->
      Error.bind (validate_selector selector) (fun () ->
          match
            ( opts.title,
              opts.daily_check_in,
              opts.task_days,
              opts.reminder_minutes )
          with
          | None, None, None, None ->
              Error
                (Error.invalid_options
                   "provide at least one goal field to update")
          | Some value, _, _, _ when nonempty value = None ->
              Error (Error.invalid_options "--title must be non-empty")
          | _, Some value, _, _ when nonempty value = None ->
              Error (Error.invalid_options "--daily-check-in must be non-empty")
          | _, _, Some value, _ when Option.is_none (task_days_mask value) ->
              Error
                (Error.invalid_options
                   "--task-days must be comma-separated weekday names")
          | _, _, _, Some value when value < 0 || value > 1439 ->
              Error
                (Error.invalid_options
                   "--reminder-minutes must be between 0 and 1439")
          | _ -> Ok ())
  | Parsed_progress (selector, day) ->
      Error.bind (validate_selector selector) (fun () ->
          match day with
          | Some value when not (valid_day value) ->
              Error (Error.invalid_options "--day must be YYYYMMDD")
          | _ -> Ok ())
  | Parsed_check_in (selector, day, status) ->
      Error.bind (validate_selector selector) (fun () ->
          match (day, status) with
          | Some value, _ when not (valid_day value) ->
              Error (Error.invalid_options "--day must be YYYYMMDD")
          | _, None ->
              Error
                (Error.invalid_options "--status must be completed or missed")
          | _ -> Ok ())
  | Parsed_pause selector | Parsed_resume selector | Parsed_archive selector ->
      validate_selector selector

let repo_or_error config =
  match config.Cli_config.repo with
  | Some repo -> Ok (repo, Cli_config.repo_to_graph repo)
  | None -> Error (Error.missing_repo "repo is required for goal")

let build ?registry:_ config (_ : Global_opts.t) = function
  | Parsed_list ->
      Error.map
        (fun (repo, graph) -> List { repo; graph })
        (repo_or_error config)
  | Parsed_show selector ->
      Error.map
        (fun (repo, graph) -> Show { repo; graph; selector })
        (repo_or_error config)
  | Parsed_create opts ->
      Error.map
        (fun (repo, graph) -> Create { repo; graph; opts })
        (repo_or_error config)
  | Parsed_update (selector, opts) ->
      Error.map
        (fun (repo, graph) -> Update { repo; graph; selector; opts })
        (repo_or_error config)
  | Parsed_delete selector ->
      Error.map
        (fun (repo, graph) -> Delete { repo; graph; selector })
        (repo_or_error config)
  | Parsed_progress (selector, day) ->
      Error.map
        (fun (repo, graph) ->
          Progress
            {
              repo;
              graph;
              selector;
              day = Option.value day ~default:(today_day ());
            })
        (repo_or_error config)
  | Parsed_check_in (selector, day, Some status) ->
      Error.map
        (fun (repo, graph) ->
          Check_in
            {
              repo;
              graph;
              selector;
              day = Option.value day ~default:(today_day ());
              status;
            })
        (repo_or_error config)
  | Parsed_check_in (_, _, None) ->
      Error (Error.invalid_options "--status must be completed or missed")
  | Parsed_pause selector ->
      Error.map
        (fun (repo, graph) ->
          Set_state
            {
              repo;
              graph;
              selector;
              state = Edn_util.keyword_t "logseq.property.goal/state.paused";
              record_kind =
                Edn_util.keyword_t "logseq.property.goal/record-kind.pause";
            })
        (repo_or_error config)
  | Parsed_resume selector ->
      Error.map
        (fun (repo, graph) ->
          Set_state
            {
              repo;
              graph;
              selector;
              state = Edn_util.keyword_t "logseq.property.goal/state.active";
              record_kind =
                Edn_util.keyword_t "logseq.property.goal/record-kind.resume";
            })
        (repo_or_error config)
  | Parsed_archive selector ->
      Error.map
        (fun (repo, graph) ->
          Set_state
            {
              repo;
              graph;
              selector;
              state = Edn_util.keyword_t "logseq.property.goal/state.archived";
              record_kind =
                Edn_util.keyword_t "logseq.property.goal/record-kind.archive";
            })
        (repo_or_error config)

let parse_edn text = Melange_edn_melange.of_edn_string text

let goals_query =
  parse_edn
    "[:find [(pull ?g [:db/id :block/uuid :block/title :block/created-at \
     {:logseq.property/description [:block/title]} \
     :logseq.property.goal/weekly-target :logseq.property.goal/weekly-unit \
     :logseq.property.goal/daily-check-in :logseq.property.goal/check-in-days \
     :logseq.property.goal/reminder-minutes :logseq.property.goal/start-day \
     {:logseq.property.goal/state [:db/ident :block/title]}]) ...] :where [?g \
     :block/tags :logseq.class/Goal]]"

let records_query =
  parse_edn
    "[:find [(pull ?r [:db/id :block/uuid :block/title \
     :logseq.property.goal/record-day :logseq.property.goal/value \
     {:logseq.property.goal/ref [:db/id :block/uuid]} \
     {:logseq.property.goal/record-kind [:db/ident]} {:logseq.property/status \
     [:db/ident]}]) ...] :where [?r :logseq.property.goal/ref] [?r \
     :logseq.property.goal/record-kind]]"

let query invoke_config repo query =
  Transport.thread_api_q invoke_config ~repo
    ~query:(Edn_util.vector_t_vec (Vec.singleton query))

let entities value = Option.value (Edn_util.as_seq value) ~default:Vec.empty
let entity_id value = Option.bind (Edn_util.get value "db/id") Edn_util.as_int64

let entity_uuid value =
  Option.bind (Edn_util.get value "block/uuid") Edn_util.as_string_like

let entity_title value =
  Option.bind (Edn_util.get value "block/title") Edn_util.as_string

let goal_state value =
  Option.bind (Edn_util.get value "logseq.property.goal/state") (fun state ->
      Option.bind (Edn_util.get state "db/ident") Edn_util.as_string_like)

let active_state = "logseq.property.goal/state.active"
let paused_state = "logseq.property.goal/state.paused"
let archived_state = "logseq.property.goal/state.archived"

let select_goal goals selector =
  Vec.filter
    (fun goal ->
      match (selector.id, selector.uuid) with
      | Some id, None -> entity_id goal = Some id
      | None, Some uuid -> entity_uuid goal = Some uuid
      | _ -> false)
    goals

let goal_not_found () = Error.make Error.Block_not_found "goal not found"

let goal_ambiguous () =
  Error.make Error.Invalid_options "goal selector is ambiguous"

let with_goal invoke_config repo selector f =
  let open Cli_effect in
  bind (query invoke_config repo goals_query) (fun value ->
      match Vec.to_array (select_goal (entities value) selector) with
      | [| goal |] -> f goal
      | [||] -> pure (Error (goal_not_found ()))
      | _ -> pure (Error (goal_ambiguous ())))

let add_action repo ~target_uuid ~title ~tags ~properties =
  let opts =
    {
      Add.target_id = None;
      target_uuid = Some target_uuid;
      target_page_name = None;
      pos = Some Block.Last_child;
      status = None;
      tags_edn = Some tags;
      properties_edn = Some properties;
      content = Some title;
      blocks_edn = None;
      blocks_file = None;
    }
  in
  Add.build_add_block_action opts Vec.empty repo

let add_action_on_page repo ~page ~title ~tags ~properties =
  let opts =
    {
      Add.target_id = None;
      target_uuid = None;
      target_page_name = Some page;
      pos = Some Block.Last_child;
      status = None;
      tags_edn = Some tags;
      properties_edn = Some properties;
      content = Some title;
      blocks_edn = None;
      blocks_file = None;
    }
  in
  Add.build_add_block_action opts Vec.empty repo

let remap_result command result = Cli_result.with_command command result

let goal_class_uuid invoke_config repo =
  Transport.thread_api_pull invoke_config ~repo
    ~selector:(Edn_util.vector_t_vec (Vec.of_array [| kw "block/uuid" |]))
    ~lookup:(kw "logseq.class/Goal")

let month_names =
  [|
    "Jan";
    "Feb";
    "Mar";
    "Apr";
    "May";
    "Jun";
    "Jul";
    "Aug";
    "Sep";
    "Oct";
    "Nov";
    "Dec";
  |]

let weekday_names =
  [|
    "Sunday"; "Monday"; "Tuesday"; "Wednesday"; "Thursday"; "Friday"; "Saturday";
  |]

let weekday_short_names = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |]

let ordinal day =
  let suffix =
    match day mod 100 with
    | 11 | 12 | 13 -> "th"
    | _ -> (
        match day mod 10 with 1 -> "st" | 2 -> "nd" | 3 -> "rd" | _ -> "th")
  in
  string_of_int day ^ suffix

let replace_all source target replacement =
  if target = "" then source
  else
    let rec loop start acc =
      match String.index_from_opt source start target.[0] with
      | None -> acc ^ String.sub source start (String.length source - start)
      | Some i
        when i + String.length target <= String.length source
             && String.sub source i (String.length target) = target ->
          loop
            (i + String.length target)
            (acc ^ String.sub source start (i - start) ^ replacement)
      | Some i -> loop (i + 1) (acc ^ String.sub source start (i - start + 1))
    in
    loop 0 ""

let pad2 value =
  if value < 10 then "0" ^ string_of_int value else string_of_int value

let journal_title format day =
  let y = day / 10000 and m = day / 100 mod 100 and d = day mod 100 in
  let date =
    Js.Date.make ~year:(float_of_int y)
      ~month:(float_of_int (m - 1))
      ~date:(float_of_int d) ()
  in
  let weekday = int_of_float (Js.Date.getDay date) in
  format |> fun s ->
  replace_all s "EEEE" "\008" |> fun s ->
  replace_all s "EEE" "\009" |> fun s ->
  replace_all s "yyyy" "\001" |> fun s ->
  replace_all s "MMM" "\002" |> fun s ->
  replace_all s "MM" "\003" |> fun s ->
  replace_all s "do" "\004" |> fun s ->
  replace_all s "dd" "\005" |> fun s ->
  replace_all s "M" "\006" |> fun s ->
  replace_all s "d" "\007" |> fun s ->
  replace_all s "\001" (string_of_int y) |> fun s ->
  replace_all s "\002" month_names.(m - 1) |> fun s ->
  replace_all s "\003" (pad2 m) |> fun s ->
  replace_all s "\004" (ordinal d) |> fun s ->
  replace_all s "\005" (pad2 d) |> fun s ->
  replace_all s "\006" (string_of_int m) |> fun s ->
  replace_all s "\007" (string_of_int d) |> fun s ->
  replace_all s "\008" weekday_names.(weekday) |> fun s ->
  replace_all s "\009" weekday_short_names.(weekday)

let string_hash value =
  let hash = ref 0L in
  String.iter
    (fun char ->
      hash :=
        Int64.logand 0xffffffffL
          (Int64.add (Int64.mul !hash 31L) (Int64.of_int (Char.code char))))
    value;
  if Int64.compare !hash 0x7fffffffL > 0 then Int64.sub !hash 0x100000000L
  else !hash

let substring_safe value start finish =
  let length = String.length value in
  if start >= length then ""
  else String.sub value start (min finish length - start)

let pad_right value length =
  value ^ String.make (max 0 (length - String.length value)) '0'

let deterministic_daily_uuid goal_uuid day =
  let key = "goal-daily-record:" ^ goal_uuid ^ ":" ^ string_of_int day in
  let hash = Int64.abs (string_hash key) |> Int64.to_string in
  let part1 = pad_right (substring_safe hash 0 4) 4 in
  let part2 = pad_right (substring_safe hash 4 8) 4 in
  let part3 = pad_right (substring_safe hash 8 12) 4 in
  let part4 = pad_right (substring_safe hash 12 (String.length hash)) 12 in
  "00000004-" ^ part1 ^ "-" ^ part2 ^ "-" ^ part3 ^ "-" ^ part4

let journal_format invoke_config repo =
  let open Cli_effect in
  bind
    (Transport.thread_api_pull invoke_config ~repo
       ~selector:
         (Edn_util.vector_t_vec
            (Vec.of_array [| kw "logseq.property.journal/title-format" |]))
       ~lookup:(kw "logseq.class/Journal"))
    (fun value ->
      pure
        (Option.value
           (Option.bind
              (Edn_util.get value "logseq.property.journal/title-format")
              Edn_util.as_string)
           ~default:"MMM do, yyyy"))

let day_weekday_bit day =
  let y = day / 10000 and m = day / 100 mod 100 and d = day mod 100 in
  let date =
    Js.Date.make ~year:(float_of_int y)
      ~month:(float_of_int (m - 1))
      ~date:(float_of_int d) ()
  in
  1 lsl int_of_float (Js.Date.getDay date)

let goal_task_days_mask goal =
  Option.value
    (Option.bind
       (Edn_util.get goal "logseq.property.goal/check-in-days")
       Edn_util.as_int)
    ~default:127

let scheduled_on_day goal day =
  goal_task_days_mask goal land day_weekday_bit day <> 0

let reminder_timestamp day minutes =
  let y = day / 10000 and m = day / 100 mod 100 and d = day mod 100 in
  let date =
    Js.Date.make ~year:(float_of_int y)
      ~month:(float_of_int (m - 1))
      ~date:(float_of_int d)
      ~hours:(float_of_int (minutes / 60))
      ~minutes:(float_of_int (minutes mod 60))
      ~seconds:0. ()
  in
  Js.Date.getTime date

let create_record config invoke_config repo goal ~day ~kind ~title ~task_status
    ~value =
  let open Cli_effect in
  match (entity_id goal, entity_uuid goal) with
  | Some goal_id, Some goal_uuid ->
      bind (journal_format invoke_config repo) (fun format ->
          let fields =
            ref
              [|
                (kw "logseq.property.goal/ref", Edn_util.int64 goal_id);
                (kw "logseq.property.goal/record-day", Edn_util.int day);
                (kw "logseq.property.goal/record-kind", Edn_util.any kind);
              |]
          in
          let values = Vec.of_array !fields in
          let values =
            match value with
            | Some v ->
                Vec.push_back values
                  (kw "logseq.property.goal/value", Edn_util.int v)
            | None -> values
          in
          let values =
            match task_status with
            | Some status ->
                Vec.push_back values
                  (kw "logseq.property/status", Edn_util.any status)
            | None -> values
          in
          let values =
            match
              ( task_status,
                Option.bind
                  (Edn_util.get goal "logseq.property.goal/reminder-minutes")
                  Edn_util.as_int )
            with
            | Some _, Some minutes ->
                Vec.push_back values
                  ( kw "logseq.property/scheduled",
                    Edn_util.float (reminder_timestamp day minutes) )
            | _ -> values
          in
          match
            add_action_on_page repo ~page:(journal_title format day) ~title
              ~tags:
                (if Option.is_some task_status then "[:logseq.class/Task]"
                 else "[:logseq.class/Goal-record]")
              ~properties:
                (Melange_edn_melange.to_edn_string (Edn_util.map_vec values))
          with
          | Error err -> pure (Error err)
          | Ok action ->
              let action =
                if
                  Edn_util.keyword_to_string kind
                  = "logseq.property.goal/record-kind.daily"
                then
                  {
                    action with
                    Add.blocks =
                      Vec.map
                        (fun block ->
                          {
                            block with
                            Block.uuid =
                              Some (deterministic_daily_uuid goal_uuid day);
                          })
                        action.blocks;
                  }
                else action
              in
              bind (Add.execute_add_block action config Output.Mode.Edn)
                (fun result ->
                  if Cli_result.is_error result then
                    pure (Error (Option.get result.Cli_result.error))
                  else pure (Ok result)))
  | _ -> pure (Error (goal_not_found ()))

let apply_property invoke_config repo uuid property value =
  let op =
    vector_vec
      (Vec.of_array
         [|
           kw "batch-set-property";
           vector_vec
             (Vec.of_array
                [|
                  vector_vec (Vec.singleton (Edn_util.uuid uuid));
                  property;
                  value;
                  Edn_util.map_vec Vec.empty;
                |]);
         |])
  in
  Transport.thread_api_apply_outliner_ops invoke_config ~repo
    ~ops:(Edn_util.vector_t_vec (Vec.singleton op))
    ~options:(Edn_util.map_t_vec Vec.empty)

let record_ref_id record =
  Option.bind (Edn_util.get record "logseq.property.goal/ref") (fun ref ->
      Option.bind (Edn_util.get ref "db/id") Edn_util.as_int64)

let record_kind record =
  Option.bind (Edn_util.get record "logseq.property.goal/record-kind")
    (fun kind ->
      Option.bind (Edn_util.get kind "db/ident") Edn_util.as_string_like)

let record_day record =
  Option.bind
    (Edn_util.get record "logseq.property.goal/record-day")
    Edn_util.as_int

let execute_create config invoke_config repo opts mode =
  let open Cli_effect in
  bind (goal_class_uuid invoke_config repo) (fun class_entity ->
      match entity_uuid class_entity with
      | None ->
          pure
            (Cli_result.error ~command:Command_id.Goal_create mode
               (Error.make Error.Target_not_found
                  "Goal class is not available; open this graph in the updated \
                   desktop app first"))
      | Some target_uuid -> (
          let title = Option.get (Option.bind opts.title nonempty) in
          let fields =
            Vec.of_array
              [|
                ( kw "logseq.property/description",
                  Edn_util.string
                    (Option.value opts.description ~default:"" |> String.trim)
                );
                (kw "logseq.property.goal/weekly-target", Edn_util.int 1);
                ( kw "logseq.property.goal/weekly-unit",
                  Edn_util.string "check-in" );
                ( kw "logseq.property.goal/start-day",
                  Edn_util.int (today_day ()) );
                ( kw "logseq.property.goal/state",
                  kw "logseq.property.goal/state.active" );
              |]
          in
          let fields =
            match Option.bind opts.daily_check_in nonempty with
            | Some value ->
                let fields =
                  Vec.push_back fields
                    ( kw "logseq.property.goal/daily-check-in",
                      Edn_util.string value )
                in
                Vec.push_back fields
                  ( kw "logseq.property.goal/check-in-days",
                    Edn_util.int
                      (Option.value
                         (Option.bind opts.task_days task_days_mask)
                         ~default:127) )
            | None -> fields
          in
          let fields =
            match opts.reminder_minutes with
            | Some value ->
                Vec.push_back fields
                  ( kw "logseq.property.goal/reminder-minutes",
                    Edn_util.int value )
            | None -> fields
          in
          match
            add_action repo ~target_uuid ~title ~tags:"[:logseq.class/Goal]"
              ~properties:
                (Melange_edn_melange.to_edn_string (Edn_util.map_vec fields))
          with
          | Error err ->
              pure (Cli_result.error ~command:Command_id.Goal_create mode err)
          | Ok action ->
              bind (Add.execute_add_block action config mode) (fun result ->
                  if Cli_result.is_error result then
                    pure (remap_result Command_id.Goal_create result)
                  else
                    match
                      ( (Vec.peek_front action.Add.blocks).Block.uuid,
                        Option.bind opts.daily_check_in nonempty )
                    with
                    | Some uuid, Some daily_title
                      when Option.value
                             (Option.bind opts.task_days task_days_mask)
                             ~default:127
                           land day_weekday_bit (today_day ())
                           <> 0 ->
                        bind
                          (with_goal invoke_config repo
                             { id = None; uuid = Some uuid } (fun goal ->
                               create_record config invoke_config repo goal
                                 ~day:(today_day ())
                                 ~kind:
                                   (Edn_util.keyword_t
                                      "logseq.property.goal/record-kind.daily")
                                 ~title:daily_title
                                 ~task_status:
                                   (Some
                                      (Edn_util.keyword_t
                                         "logseq.property/status.todo"))
                                 ~value:None))
                          (function
                            | Error err ->
                                pure
                                  (Cli_result.error
                                     ~command:Command_id.Goal_create mode err)
                            | Ok _ ->
                                pure
                                  (remap_result Command_id.Goal_create result))
                    | _ -> pure (remap_result Command_id.Goal_create result))))

let ensure_daily_record config invoke_config repo goal day =
  let open Cli_effect in
  match entity_id goal with
  | None -> pure (Error (goal_not_found ()))
  | Some _ when not (scheduled_on_day goal day) -> pure (Ok ())
  | Some goal_id ->
      bind (query invoke_config repo records_query) (fun records_value ->
          let exists =
            entities records_value
            |> Vec.exists (fun record ->
                record_ref_id record = Some goal_id
                && record_day record = Some day
                && record_kind record
                   = Some "logseq.property.goal/record-kind.daily")
          in
          if exists then pure (Ok ())
          else
            match
              Option.bind
                (Option.bind
                   (Edn_util.get goal "logseq.property.goal/daily-check-in")
                   Edn_util.as_string)
                nonempty
            with
            | None -> pure (Ok ())
            | Some title ->
                bind
                  (create_record config invoke_config repo goal ~day
                     ~kind:
                       (Edn_util.keyword_t
                          "logseq.property.goal/record-kind.daily")
                     ~title
                     ~task_status:
                       (Some (Edn_util.keyword_t "logseq.property/status.todo"))
                     ~value:None)
                  (function
                    | Error err -> pure (Error err) | Ok _ -> pure (Ok ())))

let update_goal invoke_config repo goal opts =
  let open Cli_effect in
  match entity_uuid goal with
  | None -> pure (Error (goal_not_found ()))
  | Some uuid ->
      let fields = Vec.empty in
      let fields =
        match Option.bind opts.title nonempty with
        | Some value ->
            Vec.push_back fields (kw "block/title", Edn_util.string value)
        | None -> fields
      in
      let fields =
        match Option.bind opts.daily_check_in nonempty with
        | Some value ->
            Vec.push_back fields
              (kw "logseq.property.goal/daily-check-in", Edn_util.string value)
        | None -> fields
      in
      let fields =
        match Option.bind opts.task_days task_days_mask with
        | Some value ->
            Vec.push_back fields
              (kw "logseq.property.goal/check-in-days", Edn_util.int value)
        | None -> fields
      in
      let fields =
        match opts.reminder_minutes with
        | Some value ->
            Vec.push_back fields
              (kw "logseq.property.goal/reminder-minutes", Edn_util.int value)
        | None -> fields
      in
      let rec apply remaining =
        match Vec.pop_front remaining with
        | None -> pure (Ok ())
        | Some ((property, value), rest) ->
            bind (apply_property invoke_config repo uuid property value)
              (fun _ -> apply rest)
      in
      apply fields

let delete_goal invoke_config repo goal records =
  let open Cli_effect in
  match (entity_id goal, entity_uuid goal) with
  | Some goal_id, Some goal_uuid ->
      let uuids =
        records
        |> Vec.filter (fun record -> record_ref_id record = Some goal_id)
        |> Vec.filter_map entity_uuid
        |> fun values -> Vec.push_back values goal_uuid
      in
      let op =
        vector_vec
          (Vec.of_array
             [|
               kw "delete-blocks";
               vector_vec
                 (Vec.of_array
                    [|
                      vector_vec (Vec.map Edn_util.uuid uuids);
                      Edn_util.map_vec Vec.empty;
                    |]);
             |])
      in
      bind
        (Transport.thread_api_apply_outliner_ops invoke_config ~repo
           ~ops:(Edn_util.vector_t_vec (Vec.singleton op))
           ~options:(Edn_util.map_t_vec Vec.empty))
        (fun _ -> pure (Ok ()))
  | _ -> pure (Error (goal_not_found ()))

let execute_with_mode action config mode =
  let open Cli_effect in
  let repo, command =
    match action with
    | List { repo; _ } -> (repo, Command_id.Goal_list)
    | Show { repo; _ } -> (repo, Goal_show)
    | Create { repo; _ } -> (repo, Goal_create)
    | Update { repo; _ } -> (repo, Goal_update)
    | Delete { repo; _ } -> (repo, Goal_delete)
    | Progress { repo; _ } -> (repo, Goal_progress)
    | Check_in { repo; _ } -> (repo, Goal_check_in)
    | Set_state { repo; record_kind; _ } ->
        ( repo,
          if
            Edn_util.keyword_to_string record_kind
            = "logseq.property.goal/record-kind.pause"
          then Goal_pause
          else if
            Edn_util.keyword_to_string record_kind
            = "logseq.property.goal/record-kind.resume"
          then Goal_resume
          else Goal_archive )
  in
  bind (Server_runtime.ensure_server config repo ~create_empty_db:false)
    (function
    | Error err -> pure (Cli_result.error ~command mode err)
    | Ok invoke_config -> (
        match action with
        | List _ ->
            bind (query invoke_config repo goals_query) (fun value ->
                let running =
                  entities value
                  |> Vec.filter (fun goal ->
                      goal_state goal <> Some archived_state)
                in
                pure (Cli_result.ok ~command mode (Items running)))
        | Show { selector; _ } ->
            bind
              (with_goal invoke_config repo selector (fun goal ->
                   match entity_id goal with
                   | None -> pure (Error (goal_not_found ()))
                   | Some goal_id ->
                       bind (query invoke_config repo records_query)
                         (fun records_value ->
                           let check_ins =
                             entities records_value
                             |> Vec.filter (fun record ->
                                 record_ref_id record = Some goal_id
                                 && record_kind record
                                    = Some
                                        "logseq.property.goal/record-kind.daily")
                           in
                           pure (Ok (goal, check_ins)))))
              (function
                | Error err -> pure (Cli_result.error ~command mode err)
                | Ok (goal, check_ins) ->
                    pure
                      (Cli_result.ok ~command mode
                         (Raw
                            (Edn_util.map_vec
                               (Vec.of_array
                                  [|
                                    (kw "goal", goal);
                                    (kw "check-ins", vector_vec check_ins);
                                  |])))))
        | Create { opts; _ } ->
            execute_create config invoke_config repo opts mode
        | Update { selector; opts; _ } ->
            bind
              (with_goal invoke_config repo selector (fun goal ->
                   update_goal invoke_config repo goal opts))
              (function
                | Error err -> pure (Cli_result.error ~command mode err)
                | Ok () ->
                    pure (Cli_result.ok ~command mode (Message "Goal updated")))
        | Delete { selector; _ } ->
            bind (query invoke_config repo records_query) (fun records_value ->
                bind
                  (with_goal invoke_config repo selector (fun goal ->
                       delete_goal invoke_config repo goal
                         (entities records_value)))
                  (function
                    | Error err -> pure (Cli_result.error ~command mode err)
                    | Ok () ->
                        pure
                          (Cli_result.ok ~command mode (Message "Goal deleted"))))
        | Progress { selector; day; _ } ->
            bind
              (with_goal invoke_config repo selector (fun goal ->
                   if goal_state goal <> Some active_state then
                     pure
                       (Error
                          (Error.invalid_options
                             "weekly progress can only be added to an active \
                              goal"))
                   else
                     let unit =
                       Option.value
                         (Option.bind
                            (Edn_util.get goal
                               "logseq.property.goal/weekly-unit")
                            Edn_util.as_string)
                         ~default:"times"
                     in
                     let title =
                       "Goal progress: "
                       ^ Option.value (entity_title goal) ~default:"Goal"
                       ^ " (+1 " ^ unit ^ ")"
                     in
                     create_record config invoke_config repo goal ~day
                       ~kind:
                         (Edn_util.keyword_t
                            "logseq.property.goal/record-kind.progress")
                       ~title ~task_status:None ~value:(Some 1)))
              (function
                | Error err -> pure (Cli_result.error ~command mode err)
                | Ok result -> pure (remap_result command result))
        | Check_in { selector; day; status; _ } ->
            bind
              (with_goal invoke_config repo selector (fun goal ->
                   if goal_state goal = Some archived_state then
                     pure
                       (Error
                          (Error.invalid_options
                             "an archived goal must be restored before check-in"))
                   else if not (scheduled_on_day goal day) then
                     pure
                       (Error
                          (Error.invalid_options
                             "goal has no task scheduled on that day"))
                   else
                     match entity_id goal with
                     | None -> pure (Error (goal_not_found ()))
                     | Some goal_id ->
                         bind (query invoke_config repo records_query)
                           (fun records_value ->
                             let existing =
                               entities records_value
                               |> Vec.find_opt (fun record ->
                                   record_ref_id record = Some goal_id
                                   && record_day record = Some day
                                   && record_kind record
                                      = Some
                                          "logseq.property.goal/record-kind.daily")
                             in
                             let status_ident =
                               Edn_util.keyword_t
                                 (match status with
                                 | Completed -> "logseq.property/status.done"
                                 | Missed -> "logseq.property/status.canceled")
                             in
                             match existing with
                             | Some record -> (
                                 match entity_uuid record with
                                 | None -> pure (Error (goal_not_found ()))
                                 | Some uuid ->
                                     bind
                                       (apply_property invoke_config repo uuid
                                          (kw "logseq.property/status")
                                          (Edn_util.any status_ident))
                                       (fun _ -> pure (Ok Edn_util.nil)))
                             | None -> (
                                 match
                                   Option.bind
                                     (Option.bind
                                        (Edn_util.get goal
                                           "logseq.property.goal/daily-check-in")
                                        Edn_util.as_string)
                                     nonempty
                                 with
                                 | None ->
                                     pure
                                       (Error
                                          (Error.invalid_options
                                             "goal has no daily check-in"))
                                 | Some title ->
                                     bind
                                       (create_record config invoke_config repo
                                          goal ~day
                                          ~kind:
                                            (Edn_util.keyword_t
                                               "logseq.property.goal/record-kind.daily")
                                          ~title
                                          ~task_status:(Some status_ident)
                                          ~value:None)
                                       (function
                                         | Error err -> pure (Error err)
                                         | Ok _ -> pure (Ok Edn_util.nil))))))
              (function
                | Error err -> pure (Cli_result.error ~command mode err)
                | Ok _ ->
                    pure
                      (Cli_result.ok ~command mode
                         (Message
                            (match status with
                            | Completed -> "Goal check-in completed"
                            | Missed -> "Goal check-in marked missed"))))
        | Set_state { selector; state; record_kind; _ } ->
            bind
              (with_goal invoke_config repo selector (fun goal ->
                   let kind_name = Edn_util.keyword_to_string record_kind in
                   let verb =
                     if kind_name = "logseq.property.goal/record-kind.pause"
                     then "paused"
                     else if
                       kind_name = "logseq.property.goal/record-kind.resume"
                     then "resumed"
                     else "archived"
                   in
                   let transition_error =
                     match (verb, goal_state goal) with
                     | "paused", Some state when state <> active_state ->
                         Some "only an active goal can be paused"
                     | "resumed", Some state when state <> paused_state ->
                         Some "only a paused goal can be resumed"
                     | "archived", Some state when state = archived_state ->
                         Some "goal is already archived"
                     | _, None -> Some "goal state is missing"
                     | _ -> None
                   in
                   match (transition_error, entity_uuid goal) with
                   | Some message, _ ->
                       pure (Error (Error.invalid_options message))
                   | None, None -> pure (Error (goal_not_found ()))
                   | None, Some uuid ->
                       bind
                         (create_record config invoke_config repo goal
                            ~day:(today_day ()) ~kind:record_kind
                            ~title:
                              ("Goal " ^ verb ^ ": "
                              ^ Option.value (entity_title goal) ~default:"Goal"
                              )
                            ~task_status:None ~value:None)
                         (function
                           | Error err -> pure (Error err)
                           | Ok _ ->
                               bind
                                 (apply_property invoke_config repo uuid
                                    (kw "logseq.property.goal/state")
                                    (Edn_util.any state))
                                 (fun _ ->
                                   if verb = "resumed" then
                                     bind
                                       (ensure_daily_record config invoke_config
                                          repo goal (today_day ()))
                                       (function
                                         | Error err -> pure (Error err)
                                         | Ok () -> pure (Ok verb))
                                   else pure (Ok verb)))))
              (function
                | Error err -> pure (Cli_result.error ~command mode err)
                | Ok verb ->
                    pure
                      (Cli_result.ok ~command mode (Message ("Goal " ^ verb))))))

let value ?(choices = Vec.empty) name doc =
  {
    Command_registry.names = Vec.singleton ("--" ^ name);
    arity = Required_value "value";
    doc;
    required = false;
    repeatable = false;
    choices;
    default = None;
  }

let selector_options =
  Vec.of_array [| value "id" "Goal db/id"; value "uuid" "Goal UUID" |]

let meta ?(options = Vec.empty) id doc examples =
  {
    Command_registry.id;
    path = Command_id.to_path id;
    doc;
    long_doc = None;
    examples = Vec.of_array examples;
    options;
    category = Command_registry.Graph_inspect_and_edit;
    requires_graph = Command_id.requires_graph id;
    requires_auth = Command_id.requires_auth id;
    write_command = Command_id.is_write id;
    human_table_headers_order = Vec.empty;
  }

let goal_fields =
  Vec.of_array
    [|
      value "title" "Goal name";
      value "daily-check-in" "Journal task title";
      value "task-days" "Task weekdays, comma-separated (default every day)";
      value "reminder-minutes" "Optional minutes after midnight";
    |]

let metadata () =
  Vec.of_array
    [|
      meta Command_id.Goal_list "List running goals"
        [| "logseq goal list --graph my-graph" |];
      meta ~options:selector_options Goal_show "Show a goal and its check-ins"
        [| "logseq goal show --graph my-graph --id 123" |];
      meta ~options:goal_fields Goal_create "Create a goal"
        [|
          "logseq goal create --graph my-graph --title Read --daily-check-in \
           'Read for 20 minutes' --task-days mon,wed,fri";
        |];
      meta
        ~options:(Vec.append selector_options goal_fields)
        Goal_update "Update a goal"
        [|
          "logseq goal update --graph my-graph --id 123 --task-days \
           mon,tue,wed,thu,fri";
        |];
      meta ~options:selector_options Goal_delete
        "Delete a goal and its check-in tasks"
        [| "logseq goal delete --graph my-graph --id 123" |];
      meta
        ~options:
          (Vec.append selector_options
             (Vec.of_array
                [|
                  value "day" "Journal day (YYYYMMDD), default today";
                  value
                    ~choices:(Vec.of_array [| "completed"; "missed" |])
                    "status" "Check-in outcome";
                |]))
        Goal_check_in "Complete or miss a goal check-in"
        [|
          "logseq goal check-in --graph my-graph --id 123 --status completed";
        |];
    |]

let execute action config =
  let (Output.Mode.Packed mode) = Output_mode.for_config config in
  execute_with_mode action config mode
