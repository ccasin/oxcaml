(* Real-world String Operations Benchmark
   
   Tests common patterns where String.length is frequently called,
   representing realistic application scenarios.
*)

let time_ms name f =
  let t0 = Sys.time () in
  let result = f () in
  let t1 = Sys.time () in
  let ms = (t1 -. t0) *. 1000.0 in
  Printf.printf "%-40s: %8.2f ms\n" name ms;
  result

(* Scenario 1: CSV/TSV parsing *)
let bench_csv_parsing () =
  (* Generate CSV data *)
  let rows = 10000 in
  let cols = 20 in
  let csv_data = 
    List.init rows (fun r ->
      String.concat "," (List.init cols (fun c ->
        Printf.sprintf "field_%d_%d" r c
      ))
    ) in
  
  time_ms "CSV parsing (10k rows x 20 cols)" (fun () ->
    let total_fields = ref 0 in
    let total_chars = ref 0 in
    
    List.iter (fun line ->
      let len = String.length line in  (* Check line length *)
      total_chars := !total_chars + len;
      
      (* Simple CSV split *)
      let fields = String.split_on_char ',' line in
      List.iter (fun field ->
        incr total_fields;
        total_chars := !total_chars + String.length field  (* Check field length *)
      ) fields
    ) csv_data;
    
    (!total_fields, !total_chars)
  )

(* Scenario 2: JSON-like string escaping *)
let bench_json_escaping () =
  let strings = List.init 5000 (fun i ->
    String.concat "" [
      "This is a \"test\" string ";
      string_of_int i;
      " with special chars: \n\t\r";
      String.make (i mod 100) 'x'
    ]
  ) in
  
  time_ms "JSON string escaping (5k strings)" (fun () ->
    let escaped_count = ref 0 in
    
    List.iter (fun s ->
      let len = String.length s in
      let buffer = Buffer.create (len * 2) in  (* Pre-size based on length *)
      
      for i = 0 to len - 1 do  (* Length used for bounds *)
        match s.[i] with
        | '"' -> Buffer.add_string buffer "\\\""; incr escaped_count
        | '\\' -> Buffer.add_string buffer "\\\\"; incr escaped_count
        | '\n' -> Buffer.add_string buffer "\\n"; incr escaped_count
        | '\r' -> Buffer.add_string buffer "\\r"; incr escaped_count
        | '\t' -> Buffer.add_string buffer "\\t"; incr escaped_count
        | c -> Buffer.add_char buffer c
      done;
      
      (* Check result length *)
      ignore (Buffer.length buffer)
    ) strings;
    
    !escaped_count
  )

(* Scenario 3: URL/Path manipulation *)
let bench_path_operations () =
  let paths = List.init 10000 (fun i ->
    Printf.sprintf "/home/user/project%d/src/module%d/file%d.ml" 
      (i mod 10) (i mod 100) i
  ) in
  
  time_ms "Path manipulation (10k paths)" (fun () ->
    let total_components = ref 0 in
    
    List.iter (fun path ->
      let len = String.length path in
      
      (* Find last slash *)
      let rec find_last_slash i =
        if i < 0 then -1
        else if path.[i] = '/' then i
        else find_last_slash (i - 1)
      in
      let last_slash = find_last_slash (len - 1) in
      
      (* Extract filename *)
      if last_slash >= 0 && last_slash < len - 1 then begin
        let filename = String.sub path (last_slash + 1) (len - last_slash - 1) in
        ignore (String.length filename)
      end;
      
      (* Split into components *)
      let components = String.split_on_char '/' path in
      total_components := !total_components + List.length components;
      
      (* Check each component length *)
      List.iter (fun comp ->
        ignore (String.length comp)
      ) components
    ) paths;
    
    !total_components
  )

(* Scenario 4: String validation and sanitization *)
let bench_string_validation () =
  let inputs = List.init 10000 (fun i ->
    let base = String.make (10 + i mod 90) 'a' in
    if i mod 3 = 0 then
      "  " ^ base ^ "  "  (* With whitespace *)
    else if i mod 5 = 0 then
      base ^ "\x00\x01"  (* With control chars *)
    else
      base
  ) in
  
  time_ms "String validation (10k strings)" (fun () ->
    let valid_count = ref 0 in
    let trimmed_total_len = ref 0 in
    
    List.iter (fun s ->
      let len = String.length s in
      
      (* Trim whitespace *)
      let rec trim_start i =
        if i < len && (s.[i] = ' ' || s.[i] = '\t' || s.[i] = '\n') then
          trim_start (i + 1)
        else i
      in
      let rec trim_end i =
        if i >= 0 && (s.[i] = ' ' || s.[i] = '\t' || s.[i] = '\n') then
          trim_end (i - 1)
        else i
      in
      
      let start = trim_start 0 in
      let end_ = trim_end (len - 1) in
      
      if start <= end_ then begin
        let trimmed_len = end_ - start + 1 in
        trimmed_total_len := !trimmed_total_len + trimmed_len;
        
        (* Validate no control characters *)
        let rec is_valid i =
          if i > end_ then true
          else if Char.code s.[i] < 32 then false
          else is_valid (i + 1)
        in
        
        if is_valid start then
          incr valid_count
      end
    ) inputs;
    
    (!valid_count, !trimmed_total_len)
  )

(* Scenario 5: Text search and replace *)
let bench_search_replace () =
  let documents = List.init 1000 (fun i ->
    String.concat " " [
      "The quick brown fox jumps over the lazy dog.";
      Printf.sprintf "Document number %d." i;
      "This is a test document with some repeated words.";
      "The test is testing the test framework.";
      String.make (i mod 200) '.';
    ]
  ) in
  
  time_ms "Search and replace (1k documents)" (fun () ->
    let total_replacements = ref 0 in
    
    List.iter (fun doc ->
      let len = String.length doc in
      let search = "test" in
      let search_len = String.length search in
      let replace = "example" in
      
      (* Simple search and count *)
      let rec count_occurrences pos count =
        if pos > len - search_len then count
        else
          let rec matches i =
            if i >= search_len then true
            else if doc.[pos + i] <> search.[i] then false
            else matches (i + 1)
          in
          if matches 0 then
            count_occurrences (pos + search_len) (count + 1)
          else
            count_occurrences (pos + 1) count
      in
      
      let count = count_occurrences 0 0 in
      total_replacements := !total_replacements + count;
      
      (* Build result with replacements *)
      if count > 0 then begin
        let result = Buffer.create (len + count * (String.length replace - search_len)) in
        let rec process pos =
          if pos > len - search_len then begin
            if pos < len then
              Buffer.add_substring result doc pos (len - pos)
          end else
            let rec matches i =
              if i >= search_len then true
              else if doc.[pos + i] <> search.[i] then false
              else matches (i + 1)
            in
            if matches 0 then begin
              Buffer.add_string result replace;
              process (pos + search_len)
            end else begin
              Buffer.add_char result doc.[pos];
              process (pos + 1)
            end
        in
        process 0;
        ignore (Buffer.length result)
      end
    ) documents;
    
    !total_replacements
  )

(* Scenario 6: String interning/deduplication *)
let bench_string_interning () =
  (* Generate strings with duplicates *)
  let strings = Array.init 50000 (fun i ->
    Printf.sprintf "string_%d" (i mod 1000)  (* 50x duplicates *)
  ) in
  
  time_ms "String interning (50k strings)" (fun () ->
    let table = Hashtbl.create 1000 in
    let unique_count = ref 0 in
    let total_length = ref 0 in
    
    Array.iter (fun s ->
      let len = String.length s in
      total_length := !total_length + len;
      
      (* Check if already interned *)
      try
        let existing = Hashtbl.find table s in
        ignore (String.length existing)  (* Would reuse existing *)
      with Not_found ->
        Hashtbl.add table s s;
        incr unique_count
    ) strings;
    
    (!unique_count, !total_length)
  )

(* Scenario 7: Template string formatting *)
let bench_template_formatting () =
  let templates = List.init 5000 (fun i ->
    Printf.sprintf "Hello {{name}}, your order #{{order_id}} is {{status}}. Total: ${{amount}}"
  ) in
  
  let values = [
    "name", "John Doe";
    "order_id", "12345";
    "status", "shipped";
    "amount", "99.99";
  ] in
  
  time_ms "Template formatting (5k templates)" (fun () ->
    let total_length = ref 0 in
    
    List.iter (fun template ->
      let result = Buffer.create (String.length template * 2) in
      let len = String.length template in
      
      let rec process i =
        if i >= len then ()
        else if i < len - 1 && template.[i] = '{' && template.[i+1] = '{' then begin
          (* Find closing }} *)
          let rec find_close j =
            if j >= len - 1 then len
            else if template.[j] = '}' && template.[j+1] = '}' then j
            else find_close (j + 1)
          in
          let close = find_close (i + 2) in
          if close < len then begin
            let key = String.sub template (i + 2) (close - i - 2) in
            (* Look up value *)
            begin try
              let value = List.assoc key values in
              Buffer.add_string result value;
              total_length := !total_length + String.length value
            with Not_found ->
              Buffer.add_substring result template i (close + 2 - i)
            end;
            process (close + 2)
          end else begin
            Buffer.add_char result template.[i];
            process (i + 1)
          end
        end else begin
          Buffer.add_char result template.[i];
          process (i + 1)
        end
      in
      
      process 0;
      total_length := !total_length + Buffer.length result
    ) templates;
    
    !total_length
  )

(* Main benchmark runner *)
let () =
  Printf.printf "Real-world String Operations Benchmark\n";
  Printf.printf "=======================================\n";
  Printf.printf "Testing common patterns that frequently use String.length\n\n";
  
  Printf.printf "OCaml version: %s\n" Sys.ocaml_version;
  Printf.printf "Word size: %d bits\n\n" Sys.word_size;
  
  Printf.printf "Running benchmarks...\n";
  Printf.printf "%s\n" (String.make 50 '-');
  
  (* Run all benchmarks *)
  let (fields, chars) = bench_csv_parsing () in
  Printf.printf "  -> Processed %d fields, %d total chars\n" fields chars;
  
  let escaped = bench_json_escaping () in
  Printf.printf "  -> Escaped %d characters\n" escaped;
  
  let components = bench_path_operations () in
  Printf.printf "  -> Found %d path components\n" components;
  
  let (valid, trimmed_len) = bench_string_validation () in
  Printf.printf "  -> %d valid strings, %d total trimmed length\n" valid trimmed_len;
  
  let replacements = bench_search_replace () in
  Printf.printf "  -> Made %d replacements\n" replacements;
  
  let (unique, total_len) = bench_string_interning () in
  Printf.printf "  -> %d unique strings, %d total length\n" unique total_len;
  
  let template_len = bench_template_formatting () in
  Printf.printf "  -> Generated %d total characters\n" template_len;
  
  Printf.printf "\n";
  Printf.printf "Expected improvements with header optimization:\n";
  Printf.printf "- CSV parsing: 5-10%% (many length checks)\n";
  Printf.printf "- JSON escaping: 10-15%% (length for bounds checking)\n";
  Printf.printf "- Path operations: 8-12%% (substring operations)\n";
  Printf.printf "- String validation: 10-20%% (trimming needs length)\n";
  Printf.printf "- Search/replace: 5-10%% (length for search bounds)\n";
  Printf.printf "- String interning: 15-25%% (hashtbl uses length)\n";
  Printf.printf "- Template formatting: 8-15%% (many substring ops)\n"