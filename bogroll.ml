(* #use "topfind"

 #require "netclient";;
make  && (rm -fR epub  epub.epub;./bogroll /tmp/atom.xml `pwd`/epub) && (cd epub && zip -X -0 -r -r -UN=UTF8  ../epub.epub mimetype META-INF OPS/ )

java -jar epubcheck-1.1.jar epub.epub
*)
open Http_client.Convenience
open Xml

let get_PCData = function 
  | Xml.Element(_,_,[Xml.PCData str]) -> str 
  | _ -> raise Not_found

let get_subelement ls name =
  List.find (function Xml.Element (ename, _, _) -> ename = name | _ -> false) ls
  
let get_subelement_PCData ls name =
  get_PCData (get_subelement ls name)

let filter_items ls name = 
  List.filter (function Xml.Element(ename, _, _) -> ename = name | _ -> false) ls
   
let reduce_item = function 
  | Xml.Element("entry",_, ls) -> 
      let title = get_subelement_PCData ls "title" in
      let content = get_subelement_PCData ls "content" in
      (title,content)
  | _ -> failwith "oops"

(* Returns the ls of (title, content) *)
let get_atom_ls = function 
  | Xml.Element("feed", _, ls) ->
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
  with Xml.Error (Xml.EndOfTagExpected tag as msg, pos)->
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
    Xml.Element ("p", [], [Xml.PCData text]) *)

let chaperify ls =
  let ret = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE html
  PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\">
   <head>
      <title>CHAPTER I | Pierre and Jean</title>
      <meta http-equiv=\"Content-Type\" content=\"application/xhtml+xml; charset=utf-8\"/>
      <meta name=\"EPB-UUID\" content=\"753C90B5-6C08-1014-A044-DC40CEEA30C3\"/>
   </head>

" in
  let tail = " </html>" in
  let ls = List.map (function (title, body) -> [Xml.Element( "h3", [], [Xml.PCData title]); 
                                                maybe_raw_xml body]) ls in
  let body = Xml.to_string_fmt(Xml.Element("body", [], List.flatten ls)) in
    ret ^ body ^ tail
    

let write_file dir file content =
  let _ = try Unix.mkdir (dir) 0o755 with _ -> () in
  let out = open_out (dir ^ "/" ^ file) in
    output_string out content;
    close_out out

let opf title =
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
   </metadata>
   <manifest>
    <item id=\"content\" href=\"content.xhtml\" media-type=\"application/xhtml+xml\"/>
    <item id=\"ncx\" href=\"toc.ncx\" media-type=\"application/x-dtbncx+xml\"/>
   </manifest>
   <spine toc=\"ncx\">
      <itemref idref=\"content\" linear=\"yes\"/>
   </spine>
</package>
"

let toc title =
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
  </docAuthor>
     <navMap>
      <navPoint id=\"content\" playOrder=\"1\">
         <navLabel>
            <text>"^title^"</text>
         </navLabel>
         <content src=\"content.xhtml\"/>
      </navPoint>
   </navMap>
</ncx>"

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
    write_file outdir "mimetype" "application/epub+zip";
    write_file (outdir ^ "/META-INF") "container.xml" container;
    write_file (outdir ^ "/OPS") "content.xhtml" (chaperify items);
    write_file (outdir ^ "/OPS") "content.opf"  (opf title);
    write_file (outdir ^ "/OPS") "toc.ncx"  (toc title)
;;

main Sys.argv.(1) Sys.argv.(2)
