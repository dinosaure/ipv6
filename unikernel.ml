open Utcp_miou_solo5

let echo_handler flow () =
  let finally () = TCPv4.close flow in
  Fun.protect ~finally @@ fun () ->
  let buf = Bytes.create 0x7ff in
  let rec go () =
    let len = TCPv4.read flow buf ~off:0 ~len:(Bytes.length buf) in
    if len > 0 then
      go (TCPv4.write flow (Bytes.sub_string buf 0 len)) in
  go ()

let rec clean orphans = match Miou.care orphans with
  | None | Some None -> ()
  | Some (Some prm) ->
      match Miou.await prm with
      | Ok () -> clean orphans
      | Error exn ->
          Logs.err (fun m -> m "Unexpected exception from echo task: %s"
            (Printexc.to_string exn));
          clean orphans

let run _quiet cidr gateway =
  Miou_solo5.(run [ tcpv4 ~name:"service" ?gateway cidr ]) @@ fun (daemon, tcpv4) () ->
  let rng = Mirage_crypto_rng_miou_solo5.initialize (module Mirage_crypto_rng.Fortuna) in
  let finally () =
    Mirage_crypto_rng_miou_solo5.kill rng;
    kill daemon in
  Fun.protect ~finally @@ fun () ->
  let m = TCPv4.listen tcpv4 3000 in
  let rec go orphans =
    clean orphans;
    Logs.debug (fun m -> m "waiting for a new incoming connection");
    let flow = TCPv4.accept tcpv4 m in
    let _, (ipaddr, port) = TCPv4.peers flow in
    Logs.debug (fun m -> m "new incoming connection from %a:%d" Ipaddr.pp ipaddr port);
    ignore (Miou.async ~orphans (echo_handler flow));
    go orphans in
  go (Miou.orphans ())

open Cmdliner

let ipv4 =
  let doc = "The IP address of the unikernel." in
  let ipaddr = Arg.conv (Ipaddr.V4.Prefix.of_string, Ipaddr.V4.Prefix.pp) in
  let open Arg in
  required & opt (some ipaddr) None & info [ "ipv4" ] ~doc ~docv:"IPv4"

let ipv4_gateway =
  let doc = "The IP gateway." in
  let ipaddr = Arg.conv (Ipaddr.V4.of_string, Ipaddr.V4.pp) in
  let open Arg in
  value & opt (some ipaddr) None & info [ "ipv4-gateway" ] ~doc ~docv:"IPv4"

let output_options = "OUTPUT OPTIONS"

let verbosity = Logs_cli.level ~docs:output_options ()
let renderer = Fmt_cli.style_renderer ~docs:output_options ()

let utf_8 =
  let doc = "Allow binaries to emit UTF-8 characters." in
  Arg.(value & opt bool true & info [ "with-utf-8" ] ~doc)

let t0 = Miou_solo5.clock_monotonic ()

let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over () ;
      k () in
    let with_metadata header _tags k ppf fmt =
      let t1 = Miou_solo5.clock_monotonic () in
      let delta = Float.of_int (t1 - t0) in
      let delta = delta /. 1_000_000_000. in
      Fmt.kpf k ppf
        ("[+%a][%a]%a[%a]: " ^^ fmt ^^ "\n%!")
        Fmt.(styled `Blue (fmt "%04.04f")) delta
        Fmt.(styled `Cyan int)
        (Stdlib.Domain.self () :> int)
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src) in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt in
  { Logs.report }

let setup_logs utf_8 style_renderer level =
  Option.iter (Fmt.set_style_renderer Fmt.stdout) style_renderer;
  Fmt.set_utf_8 Fmt.stdout utf_8;
  Logs.set_level level ;
  Logs.set_reporter (reporter Fmt.stdout) ;
  Option.is_none level

let setup_logs = Term.(const setup_logs $ utf_8 $ renderer $ verbosity)

let term =
  let open Term in
  const run $ setup_logs $ ipv4 $ ipv4_gateway

let cmd =
  let doc = "A simple unikernel to test the utcp." in
  let man = [] in
  let info = Cmd.info "uniker.ml" ~doc ~man in
  Cmd.v info term

let () = Cmd.(exit @@ eval cmd)
