open Unix

type command = string list

let commands_available =
  [
    ("help", "Usage: help", "Show all the available commands");
    ( "maior",
      "Usage: maior <file1> <file2>",
      "Input: two files, it tells which one is bigger\n" );
    ("setx", "setx <file>", "Adds executable mode to the file");
    ( "removerl",
      "Usage: removerl <file>",
      "Removes read from permissions of a file" );
    ( "sols",
      "Usage: sols [<dirn>]",
      "Lists all files inside a list of directories (List can be empty)" );
    ( "tipo",
      "Usage: tipo <file/directory/...>",
      "Tells if the file is a file or directory or any other type." );
    ( "calc",
      "Usage: calc <expr>",
      "Calc is a simple calculator with basic operators." );
    ( "bits",
      "Usage: bits <expr>",
      "bits is a simple calculator for bits (0 and 1) with the next operators.\n\
       | - Or\n\
       & - And\n\
       ^ - Xor\n\
       ~ - Not" );
    ("isjpeg", "Usage: isjpeg <file>", "isjpeg verify if a file is jpeg or not.");
    ( "aviso",
      "Usage: aviso <msg> <time>",
      "Aviso writes the msg on the stdout after time seconds." );
    ("quemsoueu", "Usage: quemsoueu", "quemsoueu tells my name (whoami).");
    ( "obterinfo",
      "Usage: obterinfo",
      "obterinfo writes to the stdout some info about the OS (uname -a)." );
    ( "socp",
      "Usage: socp <file1> <file2>",
      "socp copies the file1 content to the file2 (creates if not exits)." );
    ( "socpth",
      "Usage: socpth <file1> <file2>",
      "socpth copies the file1 content to the file2 (inside a new thread, \
       creates the file if not exits)." );
  ]

type num_t = VInt of int | VFloat of float
type op = Add | Sub | Mul | Div | BAnd | BOr | BXor | BNot | Val of num_t

type expr =
  | Op of num_t * op * num_t
  | Bits of num_t * op * num_t
  | BNot of int

let ( ++ ) = ( ^ )
let ( & ) = Int.logand
let ( ||| ) = Int.logor
let ( ^ ) = Int.logxor
let ( ~~ ) = Int.lognot

let expr_of_str = function
  | "+" | "add" -> Add
  | "-" | "sub" -> Sub
  | "/" | "div" -> Div
  | "*" | "mul" -> Mul
  | "|" -> BOr
  | "&" -> BAnd
  | "^" -> BXor
  | "~" -> BNot
  | t ->
      let t =
        if String.split_on_char '.' t |> List.length > 1 then
          VFloat (float_of_string t)
        else VInt (int_of_string t)
      in
      Val t

let rec apply_expr = function
  | Op (VInt n, Add, VInt m) -> VInt (n + m)
  | Op (VInt n, Sub, VInt m) -> VInt (n - m)
  | Op (VInt n, Div, VInt m) -> VInt (n / m)
  | Op (VInt n, Mul, VInt m) -> VInt (n * m)
  | Op (VFloat n, Add, VFloat m) -> VFloat (n +. m)
  | Op (VFloat n, Sub, VFloat m) -> VFloat (n -. m)
  | Op (VFloat n, Div, VFloat m) -> VFloat (n /. m)
  | Op (VFloat n, Mul, VFloat m) -> VFloat (n *. m)
  | Op (VInt n, op, VFloat m) ->
      apply_expr (Op (VFloat (float_of_int n), op, VFloat m))
  | Op (VFloat n, op, VInt m) ->
      apply_expr (Op (VFloat n, op, VFloat (float_of_int m)))
  | Bits (VInt n, op, VInt m) ->
      VInt
        (match op with
        | BAnd -> n & m
        | BOr -> n ||| m
        | BXor -> n ^ m
        | BNot -> ~~n
        | _ -> assert false)
  | BNot i -> VInt ~~i
  | _ -> assert false

let evaluate = function
  | Val n :: ((Add | Mul | Div | Sub) as op) :: Val m :: _ ->
      apply_expr (Op (n, op, m))
  | Val n :: ((BNot | BAnd | BOr | BXor) as op) :: Val m :: _ ->
      apply_expr (Bits (n, op, m))
  | Val (VInt i) :: BNot :: _ -> apply_expr (BNot i)
  | _ -> assert false

let print_num_t = function VFloat f -> print_float f | VInt n -> print_int n

type redirect =
  | In of string
  | Out of string
  | OutErr of string
  | OutAppend of string

type pipes =
  (* $ cat < main.ml 2> err.err | wc > out.txt *)
  | Command of command
  | Redirect of pipes * redirect list * pipes
  | Pipe of pipes * pipes
  | Empty

let stdout_god = dup stdout
let stding_god = dup stdin

let _string_of_command c =
  Format.sprintf "[command:(%s)]\n" (String.concat " " c)

let redirect_of_string f = function
  | "<" -> Some (In f)
  | ">" -> Some (Out f)
  | "2>" -> Some (OutErr f)
  | ">>" -> Some (OutAppend f)
  | _ -> None

let rec pipe_redirect_of_string s =
  match s with
  | [ _ ] -> Command s
  | _ ->
      let command = ref [] in
      let ended = ref false in
      let redirects = ref [] in
      let rec h = function
        | [] -> ()
        | a1 :: [] -> if not !ended then command := a1 :: !command else ()
        | a1 :: a2 :: tl ->
            if Option.is_some (redirect_of_string a2 a1) then
              let () = ended := true in
              let () =
                redirects := Option.get (redirect_of_string a2 a1) :: !redirects
              in
              h tl
            else if not !ended then
              let () = command := a1 :: !command in
              h (a2 :: tl)
            else ()
      in
      let () = h s in
      Redirect (Command (!command |> List.rev), !redirects |> List.rev, Empty)

and pipes_of_string s =
  let rec h = function
    | l :: tl ->
        let com =
          l |> String.trim |> String.split_on_char ' '
          |> pipe_redirect_of_string
        in
        Pipe (com, h tl)
    | [] -> Empty
  in
  String.split_on_char '|' s |> h

let string_of_redirect = function
  | In s -> Format.sprintf "< %s" s
  | Out s -> Format.sprintf "> %s" s
  | OutErr s -> Format.sprintf "2> %s" s
  | OutAppend s -> Format.sprintf ">> %s" s

let string_of_redirects =
  List.fold_left (fun acc l -> acc ++ string_of_redirect l) ""

let rec _string_of_pipes = function
  | Empty -> ""
  | Command c -> String.concat " " c
  | Redirect (c, r, p) ->
      Format.sprintf "%s %s | %s" (_string_of_pipes c) (string_of_redirects r)
        (_string_of_pipes p)
  | Pipe (c, Empty) -> Format.sprintf "%s" (_string_of_pipes c)
  | Pipe (c, p) ->
      Format.sprintf "%s | %s" (_string_of_pipes c) (_string_of_pipes p)

let write s =
  let o = Bytes.of_string s in
  write stdout o 0 (Bytes.length o) |> ignore

let exec c =
  let c = Array.of_list c in
  try execvp c.(0) c
  with Unix_error (err, _, _) ->
    write (Format.sprintf "Command %s: %s\n" c.(0) (error_message err))

let exec c =
  match fork () with
  | 0 ->
      handle_unix_error exec c;
      exit 0
  | _ -> (
      sleepf 0.1;
      try wait () |> ignore with _ -> ())

let prompt_text = ref "Shell"
let prompt txt = write (txt ++ " |> ")

let rec handle_pipes = function
  | Empty -> ()
  | Command c -> exec c
  | Redirect (c, r, p) ->
      let rec h = function
        | In s :: tl ->
            let fd = openfile s [ O_RDONLY ] 0 in
            dup2 fd stdin;
            close fd;
            h tl
        | Out s :: tl ->
            let fd = openfile s [ O_WRONLY; O_CREAT; O_TRUNC ] 0o666 in
            dup2 fd stdout;
            close fd;
            h tl
        | OutErr s :: tl ->
            let fd = openfile s [ O_WRONLY; O_CREAT; O_TRUNC ] 0o666 in
            dup2 fd stderr;
            close fd;
            h tl
        | OutAppend s :: tl ->
            let fd = openfile s [ O_WRONLY; O_APPEND; O_CREAT ] 0o600 in
            dup2 fd stdout;
            close fd;
            h tl
        | [] -> ()
      in
      h r |> ignore;
      handle_pipes c |> ignore;
      handle_pipes p
  | Pipe (c, Empty) -> handle_pipes c
  | Pipe (c, p) -> (
      let inf, outf = pipe () in
      match fork () with
      | 0 ->
          close inf;
          dup2 outf stdout;
          close outf;
          handle_pipes c |> ignore;
          exit 0
      | _ ->
          close outf;
          dup2 inf stdin;
          close inf;
          handle_pipes p)

let rec copy_files pos fd1 fd2 =
  try
    let b = Bytes.create 2048 in
    let n = read fd1 b pos (Bytes.length b) in
    let b = Bytes.sub b 0 n in
    let _ = Unix.write fd2 b pos (Bytes.length b) in
    copy_files (pos + 2048) fd1 fd2
  with _ -> Format.printf "Done copying files!\n@."

let commands_interpreter s =
  let s' = String.split_on_char ' ' s in
  if List.hd s' = "help" then
    let () =
      List.iter
        (fun (a, b, c) ->
          Format.printf "#########################################@.";
          Format.printf "#### Command: %s ####@." a;
          Format.printf "Usage: %s\n" b;
          Format.printf "Description: %s\n@." c)
        commands_available
    in
    let () = Format.printf "#########################################@." in
    Format.printf "Press enter key to continue!\n@." |> read_line |> ignore
  else if List.hd s' = "cd" then
    let s' = List.tl s' in
    let dir = List.hd s' in
    if dir = "~" then
      try getenv "HOME" |> chdir
      with _ ->
        Format.printf
          "Failed to find the Home directory. ($HOME is not defined)\n@."
    else chdir dir
  else if List.hd s' = "maior" then
    let tl = List.tl s' in
    if List.length tl = 2 then
      let s1 = List.hd tl in
      let s2 = List.tl tl |> List.hd in
      let stat1 = s1 |> stat in
      let stat2 = s2 |> stat in
      if stat1.st_size > stat2.st_size then
        Format.printf "File %s Bigger than %s\n@." s1 s2
      else Format.printf "File %s Bigger than %s\n@." s2 s1
    else ()
  else if List.hd s' = "setx" then
    let f = List.tl s' |> List.hd in
    (* let f_stat = stat f in *)
    (* S_IXUSR 00100 - User has execution permission *)
    chmod f 00100
  else if List.hd s' = "removerl" then
    let f = List.tl s' |> List.hd in
    chmod f 00222
  else if List.hd s' = "sols" then
    let rec read dir =
      try
        let s1 = readdir dir in
        Format.printf "Name: %s@." s1;
        let stats = s1 |> stat in
        Format.printf " | Inode: %d | Tamanho: %d\n@." stats.st_ino
          stats.st_size;
        read dir
      with
      | End_of_file -> Format.printf "End\n@."
      | _ ->
          Format.printf "\n@.";
          read dir
    in
    match List.tl s' with
    | [] ->
        let dir = "." in
        let dir_dsp = opendir dir in
        read dir_dsp
    | tl ->
        List.iter
          (fun dir ->
            Format.printf "Reading %s\n@." dir;
            let dir_dsp = opendir dir in
            read dir_dsp)
          tl
    (* let f_stat = stat f in *)
    (* nome, inode e tamanho dos ficheiros dentro de directory dir *)
  else if List.hd s' = "calc" || List.hd s' = "bits" then
    let ls = match s' with _ :: ls -> ls | [] -> [] in
    List.map expr_of_str ls |> evaluate |> print_num_t |> print_newline
  else if List.hd s' = "isjpeg" then
    let file =
      match s' with _ :: f :: _ -> f | _ :: [] | [] -> assert false
    in
    let rec hh () =
      let buff = Bytes.create 4 in
      let open Unix in
      let fdescr = openfile file [ O_RDONLY ] 0 in
      let jpgc = [ 0xff; 0xd8; 0xff; 0xe0 ] in

      let n = read fdescr buff 0 4 in
      if n <> 4 then hh ()
      else
        Bytes.fold_left
          (fun (i, bb) b -> (i + 1, bb && char_of_int (List.nth jpgc i) = b))
          (0, true) buff
    in
    if snd (hh ()) then Format.printf "Yes, the file is JPEG.@."
    else Format.printf "No, the file isn't JPEG@."
  else if List.hd s' = "aviso" then
    let s' = List.tl s' in
    let msg, time =
      let rec h acc = function
        | [ msg ] -> (fst acc, int_of_string msg)
        | a :: tl -> h (a :: fst acc, snd acc) tl
        | [] -> acc
      in
      h ([], 1) s'
    in
    Thread.create
      (fun (msg, time) ->
        sleep time |> ignore;
        Format.printf "\n%s\n@." (String.concat " " (msg |> List.rev));
        prompt !prompt_text)
      (msg, time)
    |> ignore
  else if List.hd s' = "quemsoueu" then exec [ "whoami" ]
  else if List.hd s' = "obterinfo" then exec [ "uname"; "-a" ]
  else if List.hd s' = "socp" then
    let s' = List.tl s' in
    let f1 = s' |> List.hd in
    let s' = List.tl s' in
    let f2 = s' |> List.hd in
    let fd1 = openfile f1 [ O_RDONLY ] 0 in
    let fd2 = openfile f2 [ O_WRONLY; O_CREAT; O_TRUNC ] 0o666 in
    copy_files 0 fd1 fd2
  else if List.hd s' = "tipo" then
    let s = List.tl s' in
    let f1 = s |> List.hd in
    let f1_stat = stat f1 in
    match f1_stat.st_kind with
    | S_REG -> Format.printf "Regular file!\n@."
    | S_DIR -> Format.printf "Directory!\n@."
    | _ -> Format.printf "Another type!\n@."
  else if List.hd s' = "socpth" then
    Thread.create
      (fun s ->
        let s = List.tl s in
        let f1 = s |> List.hd in
        let s = List.tl s in
        let f2 = s |> List.hd in
        let fd1 = openfile f1 [ O_RDONLY ] 0 in
        let fd2 = openfile f2 [ O_WRONLY; O_CREAT; O_TRUNC ] 0o666 in
        copy_files 0 fd1 fd2)
      s'
    |> ignore
  else
    let handle_any () =
      let p = pipes_of_string s in
      (* print_string (string_of_pipes p); *)
      handle_pipes p;
      dup2 stdout_god stdout;
      dup2 stding_god stdin;
      Format.printf "\n@.";
      sleepf 0.01
    in
    let s' = String.split_on_char '=' s |> List.map String.trim in
    match s' with
    | "PID" :: tl -> prompt_text := String.concat " " tl
    | _ -> handle_any ()

let rec loop () =
  prompt !prompt_text;
  let s = read_line () in
  let rec is_background acc = function
    | [ last ] when last = "&" -> (acc, true)
    | [] -> (acc, false)
    | a :: tl -> is_background (a :: acc) tl
  in
  let s', is_bg = is_background [] (String.split_on_char ' ' s) in
  if is_bg then
    match fork () with
    | 0 ->
        let () = commands_interpreter (String.concat " " s') in
        exit 0
    | _ -> loop ()
  else
    let () = commands_interpreter s in
    loop ()

let () = loop ()
let () = raise Thread.Exit
