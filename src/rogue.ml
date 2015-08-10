#load "graphics.cma";;
module Range = struct 
module Make(X : sig type t 
val sub : t -> t -> t 
val add : t -> t -> t 
val eq : t -> t -> bool
val min_val : t 
val min_step : t
end) =
struct 

let to_list ?(start=X.min_val) ?(step=X.min_step) stop =
let start_val = (X.sub stop X.min_step) in
let rec range_aux curr acc =
if X.eq curr start then
curr::acc
else
let next = X.sub curr step in
range_aux next (curr::acc) 
in range_aux start_val []

let iter ?(start=X.min_val) ?(step=X.min_step) stop ~f =
let stop_val = (X.sub stop X.min_step) in
let rec range_aux curr =
if X.eq curr stop_val then
f curr
else
let next = X.add curr step in
let () = f curr in
range_aux next  
in range_aux start 

let iter_cond ?(start=X.min_val) ?(step=X.min_step) stop ~f =
let stop_val = (X.sub stop X.min_step) in
let rec range_aux curr =
let res = f curr in
match res with 
|`Continue -> 
if X.eq curr stop_val then
()
else
let next = X.add curr step in
range_aux next  
|`Repeat -> 
range_aux curr
in range_aux start 

let fold ?(start=X.min_val) ?(step=X.min_step) ~init stop ~f =
let stop_val = (X.sub stop X.min_step) in
let rec range_aux curr acc =
if X.eq curr stop_val then
f acc curr
else
let next = X.add curr step in
range_aux next (f acc curr) in
range_aux start init 
end

module Int =Make(struct 
type t = int 
let sub = fun a b -> a-b
let add = fun a b -> a + b
let eq = fun a b -> a = b
let min_val = 0
let min_step = 1
end)

module Float =Make(struct 
type t = float 
let sub = fun a b -> a-.b
let add = fun a b -> a +. b
let eq = fun a b -> a = b
let min_val = 0.0
let min_step = 1.0
end)

end
let test_list = Range.Int.to_list  6 = [0;1;2;3;4;5]
let test_fold_1 = Range.Int.fold  3 ~init: 0 ~f:(fun a x -> a +x) = 3
let test_fold_2 = Range.Int.fold  3 ~init: [] ~f:(fun a x -> x::a) = [2;1;0]

module Rogue(E: sig val size : int val stuff_prob : float end )  =
struct
type attack = float
type defense = float
type luck = float
type coins = int 
type name = string 


type stuff = Wall
| Money of coins 
| Weapon of attack * name
| Shield of defense * name
| Amulet of luck * name
| Nothing

let stuff_str x = 
match x with 
|Money c -> (string_of_int c)^" coins"
|Weapon (a,n) -> n^" a sword"
|Shield(d,n) -> n^" a shield"
|Amulet (l,n) -> n^" an amulet"
|Nothing -> "nothing" 
|Wall -> "a muddy cave wall"

let stuff_chr x = 
match x with 
|Money c -> '$'
|Weapon (a,n) -> '/'
|Shield(d,n) -> '0'
|Amulet (l,n) -> '%'
|Nothing -> '.' 
|Wall -> '#'

type player = {
life : float;
attack:float;
defense:float;
luck:float;
name:string;
mutable x:int;
mutable y:int;
mutable money: int;
mutable weapon:int;
mutable shield:int;
mutable amulet:int;
mutable stuff_bag:stuff list ;
}


type enemy = {
life : float;
attack:float;
defense:float;
luck:float;
name:string;
}




module Tuple_map = Map.Make(struct 
type t = int*int 
let compare (a1,a2) (b1,b2) =

if a1 = b1 then 
compare a2 b2 
else 
compare a1 b1 
end)

let enemies = ref Tuple_map.empty


let random_script max_syl = 

let vowels = ["A";"Y";"U";"O";"AS";"YS";"US";"OS";"AR";"YR";"UR";"OR";
"OV ";"AV ";"YV ";"UV ";"OV ";"OK ";"AK ";"YK ";"UK ";"OK ";
"A'";"Y'";"U'";"O'";
"A ";"Y ";"U ";"O ";
"AS ";"YS ";"US ";"OS ";"AR ";"YR ";"UR ";"OR";
"YOA ";"YOAS ";"UOAS ";"UOA ";"UYAS ";"UYA "] in
let consonants = ["K";"L";"M";"P";"KR";"N";"KH";"GD";"DG";"DGR";
"PR";"X";"S";"B";"BR";"R";"G";"GR";"GN";
"T";"TR"] in
let particles = List.fold_left 
(fun cacc c -> 
List.fold_left
(fun vacc v -> 
(c^v)::vacc
) cacc vowels
) [] consonants 
in
let name = Range.Int.fold max_syl ~init:[] 
~f:(fun a x ->
let n = Random.int (List.length particles) in
(List.nth particles n)::a
) 
in
String.trim (String.concat "" name)

let random_name () = random_script (2+(Random.int 3))

let gen_random_cell () = 
let rnd_f = Random.float 1.0 in
if rnd_f <= E.stuff_prob then
match Random.int 10 with 
|x when x < 5 -> Money (Random.int 100)  
|x when x > 4 && x < 7 -> Weapon (Random.float 2., random_name() )
|x when x > 6 && x < 9 -> Shield (Random.float 2., random_name() )
|_ -> Amulet (Random.float 2., random_name() )

else
Nothing


let world = Array.make_matrix E.size E.size Wall 

let world_name = random_name()

let gen_player () = {
life = 100.;
attack=Random.float 1.;
defense=Random.float 1.;
luck=Random.float 1.;
name=random_name();
x=0;
y=0;
money=0;
weapon= -1;
shield= -1;
amulet= -1;
stuff_bag=[]
}

let gen_enemy () = {
life = 20. +. (Random.float 80.);
attack=Random.float 2.;
defense=Random.float 2.;
luck=Random.float 1.;
name=random_name();
}

let player = gen_player ()

let me () = print_endline ("YOU ARE "^player.name)

let peek where = print_endline 
("You see "^( 
match where with  
|`Front -> stuff_str world.(player.x).(player.y+1)
|`Back -> stuff_str world.(player.x).(player.y-1)
|`Left -> stuff_str world.(player.x-1).(player.y)
|`Right -> stuff_str world.(player.x+1).(player.y)
) )




let move who how = 
let () = print_endline "moving" in
let x,y = 
match how with  
|`Front -> who.x,who.y-1
|`Back -> who.x,who.y+1
|`Left -> who.x-1, who.y
|`Right -> who.x+1, who.y
in
if x>=0 && x< E.size &&
y >= 0 && y < E.size &&
world.(x).(y) != Wall
then begin
print_endline ((string_of_int x)^":"^(string_of_int y));
(who.x<-x;who.y<-y ) 
end 
else
print_endline "Can't walk thru walls"



(*
let init_world() = 
let () = Random.self_init () in
let correct_coord k =
if (k >= E.size) then 
k-2
else if (k < 0) then 
k+2
else 
k 
in
let  x = ref (Random.int E.size) in
let  y = ref (Random.int E.size) in
let () = player.x <- !x;
player.y <- !y in
Range.Int.iter_cond (E.size * (E.size/2) )
~f:(fun _->
(match Random.int 4 with 
|0 -> x:=correct_coord (!x+1)
|1 -> x:=correct_coord (!x-1)
|2 -> y:=correct_coord (!y+1)
|_ -> y:=correct_coord (!y-1)
);
if world.(!x).(!y) = Wall then begin
world.(!x).(!y) <- gen_random_cell();
if (Random.float 1.) <= E.stuff_prob then 
enemies := Tuple_map.add  (!x,!y) (gen_enemy ()) !enemies;
`Continue
end
else
`Repeat
) 
*)

let init_world() = 
let () = Random.self_init () in
let correct_coord k =
if (k >= E.size) then 
k-2
else if (k < 0) then 
k+2
else 
k 
in
Range.Int.iter (E.size/2) 
~f:(fun x ->
let  x = ref (Random.int E.size) in
let  y = ref (Random.int E.size) in
let () = player.x <- !x;
player.y <- !y in
Range.Int.iter_cond (E.size  )
~f:(fun _->
(match Random.int 4 with 
|0 -> x:=correct_coord (!x+1)
|1 -> x:=correct_coord (!x-1)
|2 -> y:=correct_coord (!y+1)
|_ -> y:=correct_coord (!y-1)
);
if world.(!x).(!y) = Wall then begin
world.(!x).(!y) <- gen_random_cell();
if (Random.float 1.) <= E.stuff_prob then 
enemies := Tuple_map.add  (!x,!y) (gen_enemy ()) !enemies;
`Continue
end
else
`Repeat
) 
)

let print_world () = 
Range.Int.iter E.size 
~f:(fun i ->
Range.Int.iter E.size 
~f:(fun j ->
print_char (stuff_chr world.(i).(j))
);
print_endline ""
);
print_endline ""

let print_surroundings () =
Range.Int.iter ~start:(player.x-5) (player.x+5) 
~f:(fun i -> 
Range.Int.iter ~start:(player.y-5) (player.y+5) 
~f:(fun j -> 
if i = player.x && player.y = j then
print_char '@'
else if i>=0 && i< E.size && j >= 0 && j < E.size then
print_char (stuff_chr world.(i).(j))
else
print_char '#'
);
print_endline ""
);
print_endline ""


let surroundings_matrix () =
let mat = Array.make_matrix 11 11 '.' in
let mi=ref 0 in
let mj=ref 0 in
let () = Range.Int.iter ~start:(player.x-5) (player.x+5) 
~f:(fun i -> 
mj:=0;
Range.Int.iter ~start:(player.y-5) (player.y+5) 
~f:(fun j -> 
begin
if i = player.x && player.y = j then
mat.(!mi).(!mj)<-'@'
else if i>=0 && i< E.size && 
j >= 0 && j < E.size then
mat.(!mi).(!mj)<- (stuff_chr world.(i).(j))
else
mat.(!mi).(!mj)<-'#'
end;
mj:=!mj+1
);
mi:=!mi+1
)
in
mat

end


module Game = Rogue(struct let size = 35 let stuff_prob = 0.02 end)

let draw_matrix mat sizex sizey f  = 
let module G = Graphics in 
let () = G.open_graph "";G.clear_graph() in
let x,y = G.text_size "#" in
G.resize_window sizex sizey;
Range.Int.iter sizey 
~f:(fun yi -> 
let () = G.moveto 0 ((sizey-yi)*y) in
Range.Int.iter sizex
~f:(fun xj -> 
G.draw_char (f mat.(xj).(yi))
)
)




let x() = 
let () = Game.init_world() in
draw_matrix (Game.surroundings_matrix ()) 11 11 (fun x->x);
while true do
begin
match Graphics.read_key () with 
|'w'-> Game.move Game.player `Front
|'a'->Game.move Game.player `Left
|'s'->Game.move Game.player `Back
|'d'->Game.move Game.player `Right
|_ -> ()
end;

draw_matrix (Game.surroundings_matrix ()) 11 11 (fun x->x)
done

(* test with
module Game = Rogue(struct let size = 285 let stuff_prob = 0.02 end)
let () = Game.init_world();
draw_matrix (Game.world ) 285 285 Game.stuff_chr

*)





















