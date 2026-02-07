module RNG = Mirage_crypto_rng.Fortuna
module Hash = Digestif.SHA1
let ( let@ ) finally fn = Fun.protect ~finally fn

let rng () = Mirage_crypto_rng_mkernel.initialize (module RNG)
let rng = Mkernel.map rng Mkernel.[]

let handler flow =
  let buf = Bytes.create 0x7ff in
  let rec go () =
    match Mnet.TCP.read flow buf with
    | 0 -> ()
    | len ->
      let str = Bytes.sub_string buf 0 len in
      Mnet.TCP.write flow str;
      go () in
  go ();
  Logs.debug (fun m -> m "Close the connection");
  Mnet.TCP.close flow

let rec clean_up orphans = match Miou.care orphans with
  | None | Some None -> ()
  | Some (Some prm) ->
      Miou.await_exn prm;
      clean_up orphans

let _1s = 1_000_000_000

let run _quiet cidr gateway mode =
  Mkernel.(run [ rng; Mnet.stack ~name:"service" ?gateway cidr ])
  @@ fun rng (daemon, tcp, _udp) () ->
  let@ () = fun () -> Mnet.kill daemon in
  let@ () = fun () -> Mirage_crypto_rng_mkernel.kill rng in
  match mode with
  | `Server port ->
      let rec go orphans listen =
        clean_up orphans;
        Logs.info (fun m -> m "Start to accept");
        let flow = Mnet.TCP.accept tcp listen in
        let _ = Miou.async ~orphans @@ fun () -> handler flow in
        go orphans listen in
      go (Miou.orphans ()) (Mnet.TCP.listen tcp port)
  | `Client (edn, length) ->
      let flow = Mnet.TCP.connect tcp edn in
      let@ () = fun () -> Mnet.TCP.close flow in
      let buf = Bytes.create 0x7ff in
      let rec go ctx0 ctx1 rem recv =
        if rem > 0 then begin
          Mirage_crypto_rng.generate_into buf (Bytes.length buf);
          let len = Int.min (Bytes.length buf) rem in
          Mnet.TCP.write flow (Bytes.to_string buf) ~off:0 ~len;
          let rem = rem - len in
          let ctx1 = Hash.feed_bytes ctx1 ~off:0 ~len buf in
          let len = Mnet.TCP.read flow buf in
          let ctx0 = Hash.feed_bytes ctx0 ~off:0 ~len buf in
          go ctx0 ctx1 rem (recv + len)
        end else if length - recv > 0 then
          let rec go ctx0 =
            match Mnet.TCP.read flow buf with
            | exception Mnet.Closed_by_peer | 0 -> (Hash.get ctx0, Hash.get ctx1)
            | len ->
              let ctx0 = Hash.feed_bytes ctx0 ~off:0 ~len buf in
              go ctx0 in
          Mnet.TCP.shutdown flow `write;
          go ctx0
        else (Hash.get ctx0, Hash.get ctx1) in
      let hash0, hash1 = go Hash.empty Hash.empty length 0 in
      if Hash.equal hash0 hash1 = false then exit 1;
      Fmt.pr "recv: %a\n%!" Hash.pp hash0;
      Fmt.pr "send: %a\n%!" Hash.pp hash1

let run_client _quiet cidr gateway edn length =
  run _quiet cidr gateway (`Client (edn, length))

let run_server _quiet cidr gateway port =
  run _quiet cidr gateway (`Server port)

open Cmdliner

let output_options = "OUTPUT OPTIONS"
let verbosity = Logs_cli.level ~docs:output_options ()
let renderer = Fmt_cli.style_renderer ~docs:output_options ()

let utf_8 =
  let doc = "Allow binaries to emit UTF-8 characters." in
  Arg.(value & opt bool true & info [ "with-utf-8" ] ~doc)

let t0 = Mkernel.clock_monotonic ()
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let neg fn = fun x -> not (fn x)

let reporter sources ppf =
  let re = Option.map Re.compile sources in
  let print src =
    let some re = (neg List.is_empty) (Re.matches re (Logs.Src.name src)) in
    Option.fold ~none:true ~some re
  in
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let pp header _tags k ppf fmt =
      let t1 = Mkernel.clock_monotonic () in
      let delta = Float.of_int (t1 - t0) in
      let delta = delta /. 1_000_000_000. in
      Fmt.kpf k ppf
        ("[+%a][%a]%a[%a]: " ^^ fmt ^^ "\n%!")
        Fmt.(styled `Blue (fmt "%04.04f"))
        delta
        Fmt.(styled `Cyan int)
        (Stdlib.Domain.self () :> int)
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src)
    in
    match (level, print src) with
    | Logs.Debug, false -> k ()
    | _, true | _ -> msgf @@ fun ?header ?tags fmt -> pp header tags k ppf fmt
  in
  { Logs.report }

let regexp =
  let parser str =
    match Re.Pcre.re str with
    | re -> Ok (str, `Re re)
    | exception _ -> error_msgf "Invalid PCRegexp: %S" str
  in
  let pp ppf (str, _) = Fmt.string ppf str in
  Arg.conv (parser, pp)

let sources =
  let doc = "A regexp (PCRE syntax) to identify which log we print." in
  let open Arg in
  value & opt_all regexp [ ("", `None) ] & info [ "l" ] ~doc ~docv:"REGEXP"

let setup_sources = function
  | [ (_, `None) ] -> None
  | res ->
      let res = List.map snd res in
      let res =
        List.fold_left
          (fun acc -> function `Re re -> re :: acc | _ -> acc)
          [] res
      in
      Some (Re.alt res)

let setup_sources = Term.(const setup_sources $ sources)

let setup_logs utf_8 style_renderer sources level =
  Option.iter (Fmt.set_style_renderer Fmt.stdout) style_renderer;
  Fmt.set_utf_8 Fmt.stdout utf_8;
  Logs.set_level level;
  Logs.set_reporter (reporter sources Fmt.stdout);
  Option.is_none level

let setup_logs =
  Term.(const setup_logs $ utf_8 $ renderer $ setup_sources $ verbosity)

let ipv4 =
  let doc = "The IPv4 address of the unikernel." in
  let ipaddr = Arg.conv (Ipaddr.V4.Prefix.of_string, Ipaddr.V4.Prefix.pp) in
  let open Arg in
  required & opt (some ipaddr) None & info [ "ipv4" ] ~doc ~docv:"IPv4"

let ipv4_gateway =
  let doc = "The IPv4 gateway." in
  let ipaddr = Arg.conv (Ipaddr.V4.of_string, Ipaddr.V4.pp) in
  let open Arg in
  value & opt (some ipaddr) None & info [ "ipv4-gateway" ] ~doc ~docv:"IPv4"

let port =
  let doc = "The echo server port." in
  let open Arg in
  value & opt int 9000 & info [ "p"; "port" ] ~doc ~docv:"PORT"

let length =
  let doc = "Number of bytes we would like to send." in
  let open Arg in
  value & pos 1 int 4096 & info [] ~doc ~docv:"NUMBER"

let addr =
  let doc = "The address of the echo server." in
  let parser = Ipaddr.with_port_of_string ~default:9000 in
  let pp ppf (ipaddr, port) = Fmt.pf ppf "%a:%d" Ipaddr.pp ipaddr port in
  let ipaddr_and_port = Arg.conv (parser, pp) in
  let open Arg in
  required & pos 0 (some ipaddr_and_port) None & info [] ~doc ~docv:"IP:PORT"

let term_server =
  let open Term in
  const run_server
  $ setup_logs
  $ ipv4
  $ ipv4_gateway
  $ port

let cmd_server =
  let info = Cmd.info "server" in
  Cmd.v info term_server

let term_client =
  let open Term in
  const run_client
  $ setup_logs
  $ ipv4
  $ ipv4_gateway
  $ addr
  $ length

let cmd_client =
  let info = Cmd.info "client" in
  Cmd.v info term_client

let default =
  let open Term in
  ret (const (`Help (`Pager, None)))

let () =
  let doc = "A simple echo-server as an unikernel" in
  let info = Cmd.info "echo" ~doc in
  let cmd = Cmd.group ~default info [ cmd_server; cmd_client ] in
  Cmd.(exit (eval cmd))
