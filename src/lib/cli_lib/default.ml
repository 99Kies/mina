(* Default values for cli flags *)
let work_reassignment_wait = 420000

let min_connections = 20

let max_connections = 50

let validation_queue_size = 150

let conf_dir_name = ".mina-config"

let pubsub_v1 = Gossip_net.Libp2p.RW

let pubsub_v0 = Gossip_net.Libp2p.RW
