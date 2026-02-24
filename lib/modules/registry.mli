type module_info = { exports : (string * Lunno_common.Ast.ty) list }
(** Information about a module, including its exported names and their types. *)

val register : namespace:string -> name:string -> module_info -> unit
(** [register ~namespace ~name module_info] registers a module with the given
    [namespace], [name], and [module_info].

    @param namespace The namespace of the module (e.g., "core").
    @param name The name of the module (e.g., "io").
    @param module_info The information about the module, including its exports.
*)

val find_module : namespace:string -> name:string -> module_info option
(** [find_module ~namespace ~name] looks up the module with the given
    [namespace] and [name], returning its information if found.

    @param namespace The namespace of the module (e.g., "core").
    @param name The name of the module (e.g., "io").
    @return Some module_info if the module is found, or None if not. *)
