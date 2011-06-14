(* #use "topfind"

 #require "netclient";;
make  && (rm -fR epub  epub.epub;./bogroll /tmp/atom.xml `pwd`/epub) && (cd epub && zip -X -0 -r -r -UN=UTF8  ../epub.epub mimetype META-INF OPS/ )

java -jar epubcheck-1.1.jar epub.epub
*)
open Http_client.Convenience
open Xml

let get_PCData = function 
  | Element(_,_,[Xml.PCData str]) -> str 
  | _ -> raise Not_found

let get_subelement ls name =
  List.find (function Element (ename, _, _) -> ename = name | _ -> false) ls
  
let get_subelement_PCData ls name =
  get_PCData (get_subelement ls name)

let filter_items ls name = 
  List.filter (function Element(ename, _, _) -> ename = name | _ -> false) ls
   
let reduce_item = function 
  | Element("entry",_, ls) -> 
      let title = get_subelement_PCData ls "title" in
      let content = get_subelement_PCData ls "content" in
      (title,content)
  | _ -> failwith "oops"

(* Returns  (title, [title*content]) *)
let get_atom_ls = function 
  | Element("feed", _, ls) ->
      let title = get_PCData (get_subelement ls "title") in
      let ls = filter_items ls "entry" in
      let ls = List.map reduce_item ls in
      (title, ls)
  | _ -> raise Not_found


let read f = 
  let ch = open_in f in
  let buf = String.make 4096 '0' in
  let rec read_loop ret =
    match input ch buf 0 4096 with 
        0 -> ret
      | len -> read_loop (ret ^ (String.sub buf 0 len))
  in
    read_loop ""

let rec maybe_raw_xml text =
  try 
    Xml.parse_string ("<p>" ^ text ^ "</p>")
  with Xml.Error (Xml.EndOfTagExpected tag, pos)->
    let (a, b) = Xml.abs_range pos in
    let len = String.length tag in
    let rec reverse_scan i =
      if text.[i] = '<' && String.sub text (i + 1) len = tag then
        i
      else
        reverse_scan (String.rindex_from text (i - 1) '<')
    in
    let start = reverse_scan (a - 1) in
    let tokend = String.index_from text (start + 1 + len) '>' + 1 in
    let text = String.sub text 0 tokend ^ "</" ^ tag ^ ">" ^ String.sub text tokend (String.length text - tokend) in
    maybe_raw_xml text
(*      print_string ((String.sub text start (tokend - start+len*4))^"\n");
    Element ("p", [], [Xml.PCData text]) *)

let counter = ref 1

let new_chapter_id () = 
  let ret = !counter in
    counter := !counter + 1;
    "chapter" ^ (string_of_int ret) ^ ".xhtml"
  
let chaperify (title, body) =
  let head = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE html
  PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
" in
  let id = new_chapter_id () in
  let ls = [Element( "h3", [], [Xml.PCData title]); maybe_raw_xml body] in
  let e = 
    Element ("html", [("xmlns", "http://www.w3.org/1999/xhtml"); ("xml:lang", "en")], [
               Element ("head", [], [
                          Element ("title", [], [Xml.PCData title]);
                          Element ("meta", ["http-equiv","Content-Type";"content","application/xhtml+xml; charset=utf-8"], []);
                        ]);
               Element ("body", [], ls)]
            ) in
  let body = Xml.to_string_fmt e in
    (id, head ^ body)
    

let write_file dir file content =
  let _ = try Unix.mkdir (dir) 0o755 with _ -> () in
  let out = open_out (dir ^ "/" ^ file) in
    output_string out content;
    close_out out

let opf title filenames =
  let head =
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>

<package xmlns=\"http://www.idpf.org/2007/opf\" unique-identifier=\"EPB-UUID\" version=\"2.0\">
   <metadata xmlns:opf=\"http://www.idpf.org/2007/opf\"
             xmlns:dc=\"http://purl.org/dc/elements/1.1/\">
      <dc:title>" ^ title ^ "</dc:title>
      <dc:language>en</dc:language>
      <dc:identifier id=\"EPB-UUID\">urn:uuid:TODO:uid</dc:identifier>
      <dc:creator opf:role=\"aut\" opf:file-as=\"Maupassant, Guy De\">Guy De Maupassant</dc:creator>
      <dc:publisher>epubBooks (www.epubbooks.com)</dc:publisher>
      <dc:creator opf:file-as=\"Austen, Jane\" opf:role=\"aut\">Jane Austen</dc:creator>
   </metadata>"
  in
  let ls = List.map (fun name -> Element ("item", ["id",name;"href",name;"media-type","application/xhtml+xml"],[])) filenames in
  let e = Element ("manifest", [], 
                   (Element ("item", ["id","ncx";"href","toc.ncx";"media-type","application/x-dtbncx+xml"],[]))::ls
                   ) in
  let spinels = List.map (fun name -> Element("itemref", ["idref",name;"linear","yes"], [])) filenames in
  let spine = Element ("spine", ["toc","ncx"], spinels) in
   let tail = (Xml.to_string_fmt spine) ^ "
</package>
" in
     head ^ (Xml.to_string_fmt e) ^ tail

let toc title filenames =
  let head = 
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE ncx
  PUBLIC \"-//NISO//DTD ncx 2005-1//EN\" \"http://www.daisy.org/z3986/2005/ncx-2005-1.dtd\">
<ncx version=\"2005-1\" xml:lang=\"en\" xmlns=\"http://www.daisy.org/z3986/2005/ncx/\">
   <head>
      <meta name=\"dtb:uid\" content=\"urn:uuid:TODO:uid\"/>
      <meta name=\"dtb:depth\" content=\"1\"/>
      <meta name=\"dtb:totalPageCount\" content=\"0\"/>
      <meta name=\"dtb:maxPageNumber\" content=\"0\"/>
   </head>
   <docTitle>
      <text>Pierre and Jean</text>
   </docTitle>
  <docAuthor>
    <text>Austen, Jane</text>
  </docAuthor>" in
  let _ = "
     <navMap>
      <navPoint id=\"content\" playOrder=\"1\">
         <navLabel>
            <text>"^title^"</text>
         </navLabel>
         <content src=\"content.xhtml\"/>
      </navPoint>
   </navMap>" in
  let tail = "</ncx>" in
  let counter = ref 0 in
  let new_counter () =
    counter := !counter + 1;
    string_of_int !counter
  in
  let navpoint name = 
    Element ("navPoint", ["id","content"; "playOrder",new_counter()], [
               Element ("navLabel", [], [
                          Element ("text", [], [PCData name])
                        ]);
               Element ("content", ["src",name], [])
             ])
  in
  let e = Element("navMap", [], List.map navpoint filenames) in
  head ^ (Xml.to_string_fmt e) ^ tail

let container =
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<container xmlns=\"urn:oasis:names:tc:opendocument:xmlns:container\" version=\"1.0\">
   <rootfiles>
      <rootfile full-path=\"OPS/content.opf\" media-type=\"application/oebps-package+xml\"/>
   </rootfiles>
</container>"

let main infile outdir =
  (* will take a local filename or a url *)
  let body =
    try
      read infile
    with _ ->
      http_get infile 
  in
  let (title, items) = get_atom_ls (Xml.parse_string body) in
  let items = List.map (chaperify) items in
  let file_names = fst (List.split items) in
    write_file outdir "mimetype" "application/epub+zip";
    write_file (outdir ^ "/META-INF") "container.xml" container;
    List.iter (fun (name, content) -> write_file (outdir ^ "/OPS") name  content) items;
    write_file (outdir ^ "/OPS") "content.opf"  (opf title file_names);
    write_file (outdir ^ "/OPS") "toc.ncx"  (toc title file_names)
;;

main Sys.argv.(1) Sys.argv.(2)
