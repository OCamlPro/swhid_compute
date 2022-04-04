module Make (SHA1 : sig
    val digest_string_to_hex : string -> string
  end) (OS : sig
          val contents : string -> string list option
          val typ : string -> string option
          val read_file : string -> string option
          val permissions : string -> int option
        end) =
struct

  (**/**)

  module Git = struct
    let target_type_to_git = function
      | Swhid_types.Content _hash_type -> "blob"
      | Directory -> "tree"
      | Release -> "tag"
      | Revision -> "commit"
      | Snapshot -> "refs"

    let id_to_bytes id =
      String.init
        (String.length id / 2)
        (fun i ->
            let s = String.sub id (2 * i) 2 in
            Char.chr @@ int_of_string @@ "0x" ^ s )

    let object_to_swhid (obj : string) (qualifiers : Swhid_types.qualifier list)
      mk_id : Swhid_types.identifier option =
      let hexdigest = SHA1.digest_string_to_hex obj in
      Option.map
        (fun obj -> mk_id obj qualifiers)
        (Swhid_types.object_id_from_string hexdigest)

    let object_header fmt (git_type, len) =
      match git_type with
      | "blob" | "commit" | "extid" | "raw_extrinsic_metadata" | "snapshot"
      | "tag" | "tree" ->
        Format.fprintf fmt "%s %d\x00" git_type len
      | git_type ->
        invalid_arg
          (Format.sprintf "invalid git object type `%s` (Git.object_header)"
              git_type )

    let object_from_contents_strtarget target_type contents =
      let len = String.length contents in
      Format.asprintf "%a%s" object_header (target_type, len) contents

    let object_from_contents target_type contents =
      object_from_contents_strtarget (target_type_to_git target_type) contents

    let escape_newlines snippet =
      String.concat "\n " (String.split_on_char '\n' snippet)

    let format_offset fmt (offset, negative_utc) =
      let sign =
        if offset < 0 || (offset = 0 && negative_utc) then "-" else "+"
      in
      let offset = Int.abs offset in
      let hours = offset / 60 in
      let minutes = offset mod 60 in
      Format.fprintf fmt "%s%02d%02d" sign hours minutes

    let format_author_data fmt (author, date) =
      Format.fprintf fmt "%s" author;
      match date with
      | None -> ()
      | Some (timestamp, tz_offset, negative_utc) ->
        Format.fprintf fmt " %d %a" timestamp format_offset
          (tz_offset, negative_utc)
  end

  (**/**)

  (** This module provides various functions to compute the swhid of a given
      object. Supported objects are [content], [directory], [release], [revision]
      and [snapshot]. The origins and visits objects are not supported. To learn
      more about the different object types and identifiers see the
      {{:https://docs.softwareheritage.org/devel/swh-model/data-model.html#software-artifacts}
      software heritage documentation}.*)

  (** The type for directory entries list, needed to compute directories
      identifiers. *)
  type directory_entry =
    { typ : string  (** e.g. "file", "dir" or "rev" *)
    ; permissions : int
    ; name : string
    ; target : Swhid_types.object_id
    }

  (** The type for dates, needed to compute releases and revisions identifiers. *)
  type date =
    { timestamp : int
    ; tz_offset : int
    ; negative_utc : bool
    }

  (** [content_identifier s] computes the swhid for the [s] content. [s] is the
      raw content of a file as a [string].

      E.g. [content_identifier "_build\n"] is the swhid of this library's
      [.gitignore] file. *)
  let content_identifier content : Swhid_types.identifier option =
    let git_object = Git.object_from_contents (Content "sha1_git") content in
    Git.object_to_swhid git_object [] Swhid_types.content

  (** [directory_identifier entries] compute the swhid for the [entries]
      directory. [entries] is a list of [Swhid_types.directory_entry] where each element
      points to another object (usually a file content or a sub-directory).

      E.g.
      [directory_identifier \[ { typ = "file"
                                   ; permissions = 33188
                                   ; name = "README"
                                   ; target = "37ec8ea2110c0b7a32fbb0e872f6e7debbf95e21"
                                   }\]]
      is the swhid of a directory which has a single file [README] with
      permissions 33188 and whose core identifier from [content_identifier] is
      [37ec8ea2110c0b7a32fbb0e872f6e7debbf95e21]. *)
  let directory_identifier entries : Swhid_types.identifier option =
    List.iter
      (fun entry ->
          if Swhid_types.object_id_invalid entry.target then
            invalid_arg "target must be of length 40" )
      entries;
    let entries =
      List.sort
        (fun entry1 entry2 ->
            String.compare
              (if entry1.typ = "dir" then entry1.name ^ "/" else entry1.name)
              (if entry2.typ = "dir" then entry2.name ^ "/" else entry2.name) )
        entries
    in
    let content =
      Format.asprintf "%a"
        (Format.pp_print_list
            ~pp_sep:(fun _fmt () -> ())
            (fun fmt entry ->
                Format.fprintf fmt "%o %s%c%s" entry.permissions entry.name '\x00'
                  (Git.id_to_bytes entry.target) ) )
        entries
    in
    let git_object = Git.object_from_contents Directory content in
    Git.object_to_swhid git_object [] Swhid_types.directory

  (** [directory_identifier_deep] compute the swhid for a given directory name, it uses the various functions provided in the [OS] module parameter to list directory contents, get file permissions and read file contents.*)
  let rec directory_identifier_deep name : Swhid_types.identifier option =
    match OS.contents name with
    | None -> None
    | Some contents ->
      let entries = List.map (fun name ->
        let typ = OS.typ name in
        let target = match typ with
          | Some "file" -> begin match OS.read_file name with
            | None -> None
            | Some content -> content_identifier content
          end
          | Some "directory" -> directory_identifier_deep name
          | _unknown_type -> None
        in
        let permissions = OS.permissions name in
        match typ, permissions, target with
        | Some typ, Some permissions, Some target ->
          let target = Swhid_types.get_object_id target in
          Some { typ; permissions; target; name }
        | _ -> None
      ) contents in
      if List.exists Option.is_none entries then None
      else directory_identifier (List.map Option.get entries)

  (** [release_identifier target target_type name ~author date ~message] computes
      the swhid for a release object poiting to an object of type [target_type]
      whose identifier is [target], the release having name [name], author
      [~author] and has been published on [date] with the release message
      [~message]. *)
  let release_identifier target target_type name ~author date ~message :
    Swhid_types.identifier option =
    if Swhid_types.object_id_invalid target then
      invalid_arg "target must be of length 40";

    let buff = Buffer.create 512 in
    let fmt = Format.formatter_of_buffer buff in

    Format.fprintf fmt "object %s%ctype %s%ctag %s%c" target '\n'
      (Git.target_type_to_git target_type)
      '\n' (Git.escape_newlines name) '\n';

    begin
      match author with
      | None -> ()
      | Some author ->
        Format.fprintf fmt "tagger %a%c" Git.format_author_data
          ( Git.escape_newlines author
          , Option.map (fun o -> (o.timestamp, o.tz_offset, o.negative_utc)) date
          )
          '\n'
    end;

    begin
      match message with
      | None -> ()
      | Some message -> Format.fprintf fmt "%c%s" '\n' message
    end;

    Format.pp_print_flush fmt ();

    let content = Buffer.contents buff in

    let git_object = Git.object_from_contents Release content in

    Git.object_to_swhid git_object [] Swhid_types.release

  (** [revision dir parents ~author ~author_date ~committer ~committer_date extra_headers message]
      computes the swhid for a revision object whose directory has id [dir] and
      whose parents has ids [parents] which was authored by [~author] on
      [~author_date] and committed by [~committer] on [~committer_date] with extra
      headers [extra_headers] and message [message]. *)
  let revision_identifier directory parents ~author ~author_date ~committer
      ~committer_date extra_headers message : Swhid_types.identifier option =
    if List.exists Swhid_types.object_id_invalid (directory :: parents) then
      invalid_arg "target (directory and parents) must be of length 40";

    let buff = Buffer.create 512 in
    let fmt = Format.formatter_of_buffer buff in

    Format.fprintf fmt "tree %s%c" directory '\n';

    List.iter (fun parent -> Format.fprintf fmt "parent %s%c" parent '\n') parents;

    Format.fprintf fmt "author %a%c" Git.format_author_data
      ( Git.escape_newlines author
      , Option.map
          (fun o -> (o.timestamp, o.tz_offset, o.negative_utc))
          author_date )
      '\n';

    Format.fprintf fmt "committer %a%c" Git.format_author_data
      ( Git.escape_newlines committer
      , Option.map
          (fun o -> (o.timestamp, o.tz_offset, o.negative_utc))
          committer_date )
      '\n';

    Array.iter
      (fun (k, v) -> Format.fprintf fmt "%s %s%c" k (Git.escape_newlines v) '\n')
      extra_headers;

    begin
      match message with
      | None -> ()
      | Some message -> Format.fprintf fmt "%c%s" '\n' message
    end;

    Format.pp_print_flush fmt ();

    let content = Buffer.contents buff in

    let git_object = Git.object_from_contents Revision content in

    Git.object_to_swhid git_object [] Swhid_types.revision

  (** [snapshot_identifier branches] computes the swhid of the snapshot made of
      branches [branches] where [branches] is a list of branch elements. Each
      branch is of the form [name, target] where [name] is the name of the branch
      and where [target] is a pair made of the identifier of the branch and its
      type. *)
  let snapshot_identifier (branches : (string * (string * string) option) list) :
    Swhid_types.identifier option =
    let branches =
      List.sort
        (fun (name1, _target) (name2, _target) -> String.compare name1 name2)
        branches
    in
    let buff = Buffer.create 512 in
    let fmt = Format.formatter_of_buffer buff in
    List.iter
      (fun (branch_name, target) ->
          let target, target_type, target_id_len =
            match target with
            | None -> ("", "dangling", 0)
            | Some (target, target_type) -> (
                match target_type with
                | "content" | "directory" | "revision" | "release" | "snapshot" ->
                  (Git.id_to_bytes target, target_type, 20)
                | "alias" -> (target, "alias", String.length target)
                | target_type ->
                  invalid_arg
                    (Format.sprintf
                        "invalid target type: `%s` (Compute.snapshot_identifier)"
                        target_type ) )
          in
          Format.fprintf fmt "%s %s%c%d:%s" target_type branch_name '\x00'
            target_id_len target )
      branches;
    Format.pp_print_flush fmt ();
    let content = Buffer.contents buff in
    let git_object = Git.object_from_contents_strtarget "snapshot" content in
    Git.object_to_swhid git_object [] Swhid_types.snapshot
end
