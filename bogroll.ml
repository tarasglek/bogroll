(* 
#use "topfind";;
#require "netclient";;
#require "str";;

make  && (rm -fR epub  epub.epub;./bogroll /tmp/atom.xml `pwd`/epub) && (cd epub && zip -X -0 -r -r -UN=UTF8  ../epub.epub mimetype META-INF OPS/ )

java -jar epubcheck-1.1.jar epub.epub
*)
open Http_client.Convenience
open Nethtml

let (@@) a b = function x -> a (b x);;

exception T of Nethtml.document
let get_Data = function 
  | Element(_,_,[Nethtml.Data str]) -> str 
  | e -> raise (T e)

let get_subelement ls name =
  List.find (function Element (ename, _, _) -> ename = name | _ -> false) ls

let get_subelement_Data ls name =
(*  let ls = List.filter (function Element (ename, _, _) -> ename = name | _ -> false) ls in
  let rec try_every_elemetry = function
  e::ls -> get_Data (
*)
  let e = (get_subelement ls name) in
  let ret = get_Data e in
  ret

let filter_items ls name = 
  List.filter (function Element(ename, _, _) -> ename = name | _ -> false) ls
   
type metadata = {title:string; date:string; author:string; link:string;}

(* most ghetto calendaring code ever *)
let parse_date str =
  let year::month::day::_ = List.map int_of_string (Str.split (Str.regexp_string "-") str) in
    (year-2000,month,day)

let date2days str =
  let (year,month,day) = parse_date str in
    day + (month + year * 12) * 31

let get_attr attr = function
  | Element ("link", attrs, _) -> List.assoc attr attrs 

let reduce_item = function 
  | Element("entry",_, ls) -> 
      let title = get_subelement_Data ls "title" in
      let Element (_, _, content) = get_subelement ls "content" in
      (* buggy nethtml parser leaves CDATA leftover ...also get rid of &nbsp;*)
      let rec drop_last = function
        | Data "]]>  "::[] -> []
        | Data s::ls -> 
          Data s::ls
      in
      let content = drop_last content in
      let date = String.sub (get_subelement_Data ls "published") 0 10 in
      let authorE = get_subelement ls "author" in
      let link = get_attr "href" (get_subelement ls "link") in
      let author = (try 
                      (match authorE with 
                        | Element (_,_,ls) -> get_subelement_Data ls "name")
                    with _ -> "") in
      ({title=title; date=date; author=author; link=link}, content)
  | _ -> failwith "oops"

let drop_older_items items =
  let date = try date2days ((fst @@ List.hd) items).date with _ -> 0 in
  let rec drop_older = function 
    | (metadata, item) as a::ls when date - (date2days metadata.date) <= 28 ->
        a::drop_older ls
    | _ -> [] in
  drop_older items 

(* Returns  (title, [title*content]) *)
let get_atom_ls = function 
  | Nethtml.Element("feed", _, ls) ->
      let title = get_subelement_Data ls "title" in
      let ls = filter_items ls "entry" in
      let ls = List.map reduce_item ls in
      (title, ls)
  | _ -> failwith "No root <feed> element found"

let read f = 
  let ch = open_in f in
  let buf = String.make 4096 '0' in
  let rec read_loop ret =
    match input ch buf 0 4096 with 
        0 -> ret
      | len -> read_loop (ret ^ (String.sub buf 0 len))
  in
    read_loop ""

let counter = ref 1

let new_chapter_id () = 
  let ret = !counter in
    counter := !counter + 1;
    "chapter" ^ (string_of_int ret) ^ ".xhtml"

let write_file file content = 
  let out = open_out file in
    output_string out content;
    close_out out

let write_file_to_dir dir file content =
  let _ = try Unix.mkdir (dir) 0o755 with _ -> () in
    write_file (dir ^ "/" ^ file) content

let images = ref []

let safe_http_get url =
	try
	 http_get url
	with _ -> ""

let rec fetch_images = 
  let fetch_image url =
    let name = String.copy url in
      for i = 0 to (String.length name) - 1 do
        let ch = name.[i] in
          name.[i] <- if ch = '/' or ch = ':' then '_' else ch
      done;
      let tmpname = "/tmp/" ^ name in
      let content = 
        try 
          read tmpname 
        with _ -> 
          let url = Str.global_replace (Str.regexp_string " ") "%20" url in
          let content = safe_http_get url in
            write_file tmpname content;
            content
      in
        images := (name, content)::!images;
        name
  in
  let swap_img = function 
    | ("src", url) -> ("src", fetch_image url)
    | x -> x
  in
  function
  | Element ("img", attrls, childls) -> 
      (try 
         Element ("img", List.map swap_img attrls, childls)
       with Http_client.Http_error _ -> Data "[Missing image]")
  | Data _ as p -> p
  | Element (a,b,ls) -> Element (a, b, List.map fetch_images ls)


let xml_to_string e = 
  let buf = Buffer.create 1 in
  let ob = new Netchannels.output_buffer buf in
  let _ = (Nethtml.write ~xhtml:true ob(* @@ Nethtml.encode ~prefer_name:false*))[e] in
  Buffer.contents buf

let string_to_html = 
  List.hd @@ Nethtml.parse @@ new Netchannels.input_string

let chaperify (metadata, body) =
  let head = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE html
  PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
" in
  let id = new_chapter_id () in
  let ls = Element( "h3", [], [Element ("a", ["href", metadata.link], [Data metadata.title])])::
    Element( "p", [], [Element ("i", [], [Data (metadata.date ^ " " ^ metadata.author)])])::
    [Element ("p", [], body)] in
  let e = 
    Element ("html", [("xmlns", "http://www.w3.org/1999/xhtml"); ("xml:lang", "en")], [
               Element ("head", [], [
                          Element ("title", [], [Data metadata.title]);
                          Element ("meta", ["http-equiv","Content-Type";"content","application/xhtml+xml; charset=utf-8"], []);
                        ]);
               Element ("body", [], ls)]
            ) in
  let e = fetch_images e in
  let body = xml_to_string e in
    (id, head ^ body)

let get_mime_type filename =
  let dot = (String.rindex filename '.') + 1 in
  let ext = String.lowercase (String.sub filename dot (String.length filename - dot)) in
    match ext with 
      | "xhtml" -> "application/xhtml+xml"
      | "ncx" -> "application/x-dtbncx+xml"
      | "jpg" -> "image/jpeg"
      | "png" -> "image/png"
      | "gif" -> "image/gif"
      | x -> "image/jpeg" 
(*
(print_endline x;raise Not_found)*)
    

let opf id title filenames imagefilenames =
  let head =
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>

<package xmlns=\"http://www.idpf.org/2007/opf\" unique-identifier=\"EPB-UUID\" version=\"2.0\">
   <metadata xmlns:opf=\"http://www.idpf.org/2007/opf\"
             xmlns:dc=\"http://purl.org/dc/elements/1.1/\">
      <dc:title>" ^ title ^ "</dc:title>
      <dc:language>en</dc:language>
      <dc:identifier id=\"EPB-UUID\">"^id^"</dc:identifier>
      <dc:creator opf:role=\"aut\">Bog Roll</dc:creator>
   </metadata>"
  in
  let map_element_mime = 
    List.map (fun name -> Element ("item", ["id",name;"href",name;"media-type",get_mime_type name],[]))
  in
  let ls = map_element_mime (List.append filenames imagefilenames) in
  let e = Element ("manifest", [], 
                   (Element ("item", ["id","ncx";"href","toc.ncx";"media-type","application/x-dtbncx+xml"],[]))::ls
                   ) in
  let spinels = List.map (fun name -> Element("itemref", ["idref",name;"linear","yes"], [])) filenames in
  let spine = Element ("spine", ["toc","ncx"], spinels) in
   let tail = (xml_to_string spine) ^ "
</package>
" in
     head ^ (xml_to_string e) ^ tail

let toc id title filenames =
  let head = 
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE ncx
  PUBLIC \"-//NISO//DTD ncx 2005-1//EN\" \"http://www.daisy.org/z3986/2005/ncx-2005-1.dtd\">
<ncx version=\"2005-1\" xml:lang=\"en\" xmlns=\"http://www.daisy.org/z3986/2005/ncx/\">
   <head>
      <meta name=\"dtb:uid\" content=\""^id^"\"/>
      <meta name=\"dtb:depth\" content=\"1\"/>
      <meta name=\"dtb:totalPageCount\" content=\"0\"/>
      <meta name=\"dtb:maxPageNumber\" content=\"0\"/>
   </head>
  <docAuthor>
    <text>Bog Roll</text>
  </docAuthor>" in
  let tail = "</ncx>" in
  let counter = ref 0 in
  let new_counter () =
    counter := !counter + 1;
    string_of_int !counter
  in
  let navpoint (title, name) = 
    Element ("navPoint", ["id","content"; "playOrder",new_counter()], [
               Element ("navLabel", [], [
                          Element ("text", [], [Data title])
                        ]);
               Element ("content", ["src",name], [])
             ])
  in
  let e = Element("navMap", [], List.map navpoint filenames) in
  head ^ (xml_to_string e) ^ tail

let container =
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<container xmlns=\"urn:oasis:names:tc:opendocument:xmlns:container\" version=\"1.0\">
   <rootfiles>
      <rootfile full-path=\"OPS/content.opf\" media-type=\"application/oebps-package+xml\"/>
   </rootfiles>
</container>"

let run args =
    let _ = Unix.create_process args.(0) args Unix.stdin Unix.stdout Unix.stderr in
    let _ = Unix.wait () in
      ()

let publish outdir id title items =
  let outfile = outdir ^ "/" ^ title ^ ".epub" in
  let outdir = outfile ^ ".tmp" in
  let titles = List.map (fun x -> x.title) (fst (List.split items)) in
  let items = List.map (chaperify) items in
  let file_names = fst (List.split items) in
  let titles2file_names = List.combine titles file_names in
    write_file_to_dir outdir "mimetype" "application/epub+zip";
    write_file_to_dir (outdir ^ "/META-INF") "container.xml" container;
    List.iter (fun (name, content) -> write_file_to_dir (outdir ^ "/OPS") name content) items;
    List.iter (fun (name, content) -> write_file_to_dir (outdir ^ "/OPS") name content) !images;
    write_file_to_dir (outdir ^ "/OPS") "content.opf"  (opf id title file_names (fst (List.split !images)));
    write_file_to_dir (outdir ^ "/OPS") "toc.ncx"  (toc id title titles2file_names);
    let oldcwd = Unix.getcwd() in
    let outfile = if outfile.[0] = '/' then outfile else oldcwd ^ "/" ^ outfile in
    Unix.chdir outdir;
    (try 
      Unix.unlink outfile
    with _ -> ());
    let args = [|"zip"; "-0"; "-X"; outfile; "mimetype"|] in
    run args;
    args.(Array.length args - 1) <- "-r";
    args.(1) <- "-9" ;
    let args = Array.append args [|"META-INF"; "OPS"|] in
    run args;
    Unix.chdir oldcwd;
    run [|"rm"; "-r"; outdir|];
(*    (cd epub && zip -X -0 -r -r -UN=UTF8  ../epub.epub mimetype META-INF OPS/ ) *)
    print_endline ("finished " ^ outfile)

let url2content infile = 
  print_endline ("Processing " ^ infile);
  (* will take a local filename or a url *)
  let body =
    try
       read infile
     with _ ->
       safe_http_get infile 
   in
   let (title, items) = get_atom_ls (string_to_html body) in
     List.map (fun (metadata, content) -> ({metadata with author = (metadata.author ^ " | " ^ title)}, content)) items

let go atomls outdir =
  let items = (List.concat @@ (List.map url2content)) atomls in  
  let rev_compare (m1,_) (m2,_) = 
    date2days m2.date - date2days m1.date
  in
  let items = List.sort rev_compare items in
  let items = drop_older_items items in
  let id = "bogroll" in
    publish outdir id "Economist Blogs" items
;;

Printexc.record_backtrace true;
let _::outdir::atomls = Array.to_list Sys.argv in
go atomls outdir;
let _ = exit 0 in
Printexc.print_backtrace stdout
