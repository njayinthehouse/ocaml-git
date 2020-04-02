(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(** Unix backend. *)

module Fs = Fs
module Cass_fs = Cass.Fs
module Net = Net
module Index = Index

module Store : sig
  include
    Git.Store.S
    with module Hash = Git.Hash.Make(Digestif.SHA1)
     and module FS := Fs

  val v :
       ?dotgit:Fpath.t
    -> ?compression:int
    -> ?buffer:((buffer -> unit Lwt.t) -> unit Lwt.t)
    -> Fpath.t
    -> (t, error) result Lwt.t
end

module Cass_store : sig
  include 
    Git.Store.S
    with module Hash = Git.Hash.Make (Digestif.SHA1)
     and module FS := Cass_fs

  val v :
      ?dotgit:Fpath.t
   -> ?compression:int
   -> ?buffer:((buffer -> unit Lwt.t) -> unit Lwt.t)
   -> Fpath.t
   -> string 
   -> (t, error) result Lwt.t

  val closeSession : Cass.cassSession -> unit
  val closeCluster : Cass.cassCluster -> unit
end

type endpoint = Net.endpoint = {uri: Uri.t; headers: Cohttp.Header.t}

val endpoint : ?headers:Cohttp.Header.t -> Uri.t -> endpoint

module Sync (G : Git.S) : sig
  module Tcp : Git.Sync.S with module Store := G and type Endpoint.t = endpoint

  module Http :
    Git_http.Sync.S with module Store := G and type Client.endpoint = endpoint

  include Git.Sync.S with module Store := G and type Endpoint.t = endpoint
end
