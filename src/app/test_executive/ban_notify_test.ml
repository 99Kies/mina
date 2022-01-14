open Core
open Async
open Integration_test_lib

module Make (Inputs : Intf.Test.Inputs_intf) = struct
  open Inputs
  open Engine
  open Dsl

  (* TODO: find a way to avoid this type alias (first class module signatures restrictions make this tricky) *)
  type network = Network.t

  type node = Network.Node.t

  type dsl = Dsl.t

  let config =
    let open Test_config in
    let open Test_config.Block_producer in
    { default with
      requires_graphql = true
    ; block_producers =
        [ { balance = "1000"; timing = Untimed }
        ; { balance = "1000"; timing = Untimed }
        ; { balance = "8000"; timing = Untimed }
        ]
    ; num_snark_workers = 0
    }

  let run network t =
    let open Network in
    let open Malleable_error.Let_syntax in
    let logger = Logger.create () in
    (* TEMP: until we fix the seed graphql port, we will only check peers for block producers *)
    (* let all_nodes = Network.all_nodes network in *)
    let all_nodes = Network.block_producers network in
    let[@warning "-8"] [ alice; bob; carol ] =
      Network.block_producers network
    in
    (* TODO: let%bind () = wait_for t (Wait_condition.nodes_to_initialize [node_a; node_b; node_c]) in *)
    let%bind () =
      Malleable_error.List.iter [ alice; bob; carol ]
        ~f:(Fn.compose (wait_for t) Wait_condition.node_to_initialize)
    in
    let%bind () =
      section "network is fully connected upon initialization"
        (Util.check_peers ~logger all_nodes)
    in
    let%bind _ =
      section "blocks are produced"
        (wait_for t (Wait_condition.blocks_to_be_produced 1))
    in
    let%bind () =
      section "restart one node with time set into the future"
        (let%bind () = Node.stop carol in
         [%log info] "%s stopped, will now wait for blocks to be produced"
           (Node.id carol) ;
         let%bind _ = wait_for t (Wait_condition.blocks_to_be_produced 1) in
         let%bind () = Node.start ~fresh_state:false ~env_vars:[
           ("EXTRA_ENV", "MINA_TIME_OFFSET=-600")
         ] carol in
         [%log info]
           "%s started again, will now wait for this node to initialize"
          (Node.id carol) ;
         Malleable_error.ok_unit)
    in
    section "network isn't fully connected after one node was restarted" (
      let%bind () = Malleable_error.lift (after (Time.Span.of_sec 240.0)) in
      let%bind _ = wait_for t (Wait_condition.blocks_to_be_produced 3) in
      Malleable_error.reverse_ok_error ~preserve_soft:false
       (Error.of_string "network is full connected")    
       (Util.check_peers ~logger all_nodes)
  )
end
