(*

  "datasource"
  Copyright (C) 2010-2013 Francesco Tovagliari

  This file is part of "datasource".

  "datasource" is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  "datasource" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.

*)

module type POOLED_OBJECT =
  sig
    type p
    type t
    val create : p -> t
    val is_valid : t -> bool
    val destroy : t -> unit
  end

module type S =
  sig
    type pobj
    type param
    type pool
    val verbose : int ref
    val create : ?initial_size:int -> ?max_size:int -> ?expiration_time:float -> param -> pool
    val checkin : pool -> pobj -> unit
    val checkout : pool -> pobj
    val length : pool -> int * int
    val clean_up : pool -> unit
    val destroy : pool -> unit
  end

module Make (PObj : POOLED_OBJECT) =
  struct
    type pobj = PObj.t
    type param = PObj.p

    let verbose = ref 0

    module HashedPoolObject =
      struct
        type t = pobj
        let equal x y = (x == y)
        let hash x = Hashtbl.hash x
      end

    module POHashtbl = Hashtbl.Make(HashedPoolObject)

    type pool = {
      id               : int;
      locked           : ((*pobj, *)float) POHashtbl.t;
      (** Oggetti in uso: tabella Hash che ha come chiave l'oggetto e come dato il tempo
          di estrazione dal pool. *)
      mutable unlocked : (float * pobj) list;
      (** Oggetti liberi: lista di coppie con tempo di inserimento nel pool
          (tempo di rilascio) ed oggetto *)
      expire           : float option;
      param            : param;
      max_size         : int
    }

    exception Full_pool

    (** Dimensioni del pool *)
    let length_locked pool = POHashtbl.length pool.locked

    let length_unlocked pool = List.length pool.unlocked

    let length pool = (POHashtbl.length pool.locked), (List.length pool.unlocked)

    (** Weak table *)
    module OrderedPool =
      struct
        type t = pool
        let equal x y = (x == y)
        let hash x = Hashtbl.hash x
      end

    module WeakTable = Weak.Make (OrderedPool)

    let expire_table = WeakTable.create 7

    let thread_cleaner = ref false

    let m_unlocked = Mutex.create ()

    (** Debug *)
    let print fn ?msg pool =
      let msg = match msg with None -> "" | Some m -> "\n\t" ^ m in
      Printf.printf "Pool.%s (pool-id=%d): %d+%d (locked+unlocked) %s\n%!"
        fn pool.id (length_locked pool) (length_unlocked pool) msg

    (** clean_up *)
    let clean_up pool =
      match pool.expire with
        | None -> ()
        | Some expire ->
          let count = ref 0 in
          try
            Mutex.lock m_unlocked;
            let current_time = Unix.gettimeofday () in
            pool.unlocked <- List.filter begin function (ts, pobj) ->
                if current_time -. ts >= expire then begin
                  PObj.destroy pobj;
                  incr count;
                  false
                end else true
              end pool.unlocked;
            Mutex.unlock m_unlocked;
            if !verbose = 1 && !count > 0 then (print "clean_up" ~msg:(Printf.sprintf "%d elements destroyed." !count) pool)
          with ex ->
            Mutex.unlock m_unlocked;
            Printf.eprintf "File \"pool.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
            raise ex

    (** La funzione eseguita dal thread di pulitura. *)
    let cleaner () =
      while true do
        Thread.delay 0.5;
        WeakTable.iter clean_up expire_table
      done

    (** Checkin *)
    let checkin pool pobj =
      POHashtbl.remove pool.locked pobj;
      Mutex.lock m_unlocked;
      pool.unlocked <- (Unix.gettimeofday(), pobj) :: pool.unlocked;
      Mutex.unlock m_unlocked;
      (if !verbose = 1 then print "checkin" pool)

    (** Checkout  *)
    let rec checkout pool =
      let time, elem =
        try
          Mutex.lock m_unlocked;
          let elem =
            match pool.unlocked with
              | [] ->
                let l1, l2 = length pool in
                if l1 + l2 >= pool.max_size then (raise Full_pool);
                (Unix.gettimeofday()), PObj.create pool.param;
              | head :: tail ->
                (* Se l'oggetto estratto è già scaduto si può:
                   a) ignorarlo e cercarne uno non scaduto (lasciando al cleaner il compito di distruggerlo);
                   b) prenderlo lo stesso.
                   c) distruggerlo (anticipando il cleaner) e cercarne uno non scaduto;
                   b) e c) vanno bene se le risorse sono a basso costo o "deperibili".
                   PS. Meglio non anticipare il cleaner che ripulisce tutto quando il carico di
                   sistema lo permette.
                   Al contrario b) va bene se lo scopo è quello di non lasciare inutilizzate
                   risorse costose da ricostruire.
                *)
                pool.unlocked <- tail;
                head
          in
          Mutex.unlock m_unlocked;
          elem
        with ex ->
          Mutex.unlock m_unlocked;
          raise ex
      in
      let not_expired = match pool.expire with None -> true
        | Some expiration_time -> (Unix.gettimeofday()) -. time < expiration_time
      in
      if not_expired && PObj.is_valid elem then begin (* TODO: is_valid = true, sempre *)
        POHashtbl.add pool.locked elem (Unix.gettimeofday());
        (if !verbose = 1 then print "checkout" pool);
        elem
      end else begin
        (* TODO: La distruzione si potrebbe post-porre accodando gli oggetti e
          delegando il thread cleaner dell'incarico. *)
        PObj.destroy elem;
        (if !verbose = 1 then print "checkout" pool);
        checkout pool (* Ne tiro fuori un altro *)
      end

    (** destroy
        Forza un checkin degli oggetti locked e distrugge tutti gli oggetti unlocked. *)
    let destroy pool =
      clean_up pool;
      POHashtbl.iter (fun pobj _ -> checkin pool pobj) pool.locked;
      Mutex.lock m_unlocked;
      List.iter (fun (_, pobj) -> PObj.destroy pobj) pool.unlocked;
      pool.unlocked <- [];
      Mutex.unlock m_unlocked;
      (if !verbose = 1 then print "destroy" ~msg:(Printf.sprintf "expire_table length=%d" (WeakTable.count expire_table)) pool)

    (** next_id *)
    let next_id =
      let seq = ref 0 in fun () -> (incr seq; !seq)

    (** Create *)
    let create ?(initial_size=0) ?(max_size=max_int) ?expiration_time param =
      if initial_size > max_size then (raise Full_pool);
      let pool = {
        id       = next_id();
        locked   = POHashtbl.create 7;
        unlocked = [];
        expire   = expiration_time;
        param    = param;
        max_size = max_size
      } in
      for _i = 1 to initial_size do checkin pool (PObj.create pool.param) done;
      (* Prima di distruggere il pool distruggo tutti gli elementi che contiene. *)
      Gc.finalise destroy pool;
      (* Se gli oggetti del pool hanno una scadenza lo registro nella weak table in modo da
        poterlo pulire. *)
      begin match expiration_time with
        | None -> ()
        | Some _time ->
          WeakTable.add expire_table pool;
          if not !thread_cleaner then begin
            ignore (Thread.create cleaner ());
            thread_cleaner := true
          end;
      end;
      pool

  end




