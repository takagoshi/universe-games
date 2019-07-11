open UniverseJs
open World
open Image
open Color

let time_per_second = 4

(*制限時間*)
let time_limit = 241

(* 乱数の初期化 *)
(* ;;if truly_random then Random.self_init () else Random.init 0 *)

let rate : float = (1. /. (float_of_int time_per_second))

type basket_t = (int * int) * int * string

(* world_t : カゴの座標, 玉の座標, スコア, 制限時間 の型 *)
type world_t = { basket : basket_t;
                 ball : int * int;
                 score : int;
                 time : int;
               }

(* constants *)
let width = 1000	(* 画面の幅 *)
let height = 600      (* 画面の高さ *)

let basket_width = 80
let basket_height = 100
let ball_width = 30

let background = empty_scene (float_of_int width) (float_of_int height)
let basket = rectangle (float_of_int basket_width) (float_of_int basket_height) (make_color 90 80 130)
let ball = circle (float_of_int ball_width) (make_color 210 120 10)

(* -----初期値関係----- *)

(* カゴと玉の動く幅 *)
let ball_y  = 10
let basket_x = 10

(* worldの初期値 *)
let initial_world ={ basket = ((500, basket_height), basket_x, "through");
                     ball = (15, 550);
                     score = 0;
                     time = 0;
		   }


(* -----スコアの判定関係-----*)

(* 点数 *)
let score_10 = 10

(* 玉とカゴの座標をもらう *)
(* 玉がカゴに入ったら score、入らなかったら 0 を返す *)
let check world = match world with
    { basket = ((ax, ay), vct, str); ball = (x, y); score = scr; time = time_v }
    -> if ax <= x && x + ball_width <= ax + basket_width && y < basket_height - ball_width
    then score_10
    else 0


(* -----キー操作関係-----*)

let key_draw world key =  match world with
  { basket = basket_v; ball = (x, y); score = scr; time = time_v } ->
  if key = "left" then
    { basket = basket_v; ball = (x - 10, y); score = scr; time = time_v }
  else if key = "right" then
    { basket = basket_v; ball = (x + 10, y); score = scr; time = time_v }
  else if key = "down" then
    { basket = basket_v; ball = (x, y - 10); score = scr; time = time_v }
  else world


(* -----on_tick関係----- *)

let new_ball x = Random.int 941

(* 果物の y 座標が一番下についていたら一番上にもってくる *)
(* up_fruit : int -> int *)
let up_ball (x, y) =
  if (y < basket_height - ball_width) then ((new_ball x) + ball_width, height) else (x, y)

(* ボールを下に動かす *)
(* move_ball : int * int -> int * int *)
let move_ball (x, y) = (x, y - ball_y)

(* カゴを左右に動かす(端までいったら反対方向に動く) *)
(* 壁 と ball がぶつかっていたら 新たな方向ベクトルをセット *)
  (* reflect_w : ball_t -> ball_t *)
let reflect_w ((ax, ay), ix, str) =
    if ax + basket_width >=  width then ((ax, ay), -ix, "reflect right wall")
    else if ax <= 0 then ((ax, ay), -ix, "reflect left wall")
    else ((ax, ay), ix, str)
      
let rec move_basket bas = match bas with ((ax, ay), ix, str) ->
  reflect_w ((ax + ix, ay), ix, str)


(* ボールを下に動かす *)
(* ボールが下についたら上にセット *)
(* ボールがカゴに入ったらscoreを加算 *)
(* カゴは右端までいったら左に移動、左端までいったら右に移動 *)
(* move_on_tick : world_t -> (world_t, 'a) World.t *)
let move_on_tick world = match world with
    { basket = basket_v; ball = (x, y); score = scr; time = time_v }
    -> { basket = (move_basket basket_v);
         ball = move_ball (up_ball (x, y));
         score = scr + check world;
         time = time_v }


(* -----描画関係----- *)

(* y座標を表示用に変換 *)
(* change_y : (int * int) -> int *)
let change_y y = height - y
let change_x x = width - x

(* rate 単位の経過時間を受け取って、残り秒数を返す *)
let time_to_second time = (time_limit - time) / time_per_second 

(* 各座標から画像を作成 *)
let draw world =  match world with
    { basket = ((ax, ay), ix, str); ball = (x, y); score = scr; time = time_v }
    -> let time_str = string_of_int (time_to_second time_v) in
    (place_images
       [text (string_of_int scr) ~size:50 black;
        text time_str ~size:50 black;
        text "Score" ~size:30 black;
        text "Time" ~size:30 black]
       [(45., 35.);
        (float_of_int (change_x 70), float_of_int (change_y 560));
        (25., 10.);
        (float_of_int (change_x 80), float_of_int (change_y 590))]
       (place_image basket (float_of_int ax,  float_of_int (change_y ay))
          (place_image ball (float_of_int x, float_of_int (change_y y))       
             background)))

let draw_game_over (world:world_t) =
  place_image (text "TIME UP" ~size:150 black)
              (float_of_int (70), float_of_int (200))
              (draw world)

let game_finished ({time}:world_t) = time_limit <= time


(* ----毎時処理関係---- *)
let inc_time (world:world_t) =  {world with time = world.time + 1}

let on_tick (world:world_t) =
  move_on_tick (inc_time world)
  (* let world = inc_time world in  (\* 時間を増やすだけ *\)
   * let moved_world = move_on_tick world in
   * moved_world *)

(* ゲーム開始 *)
let _ =
  big_bang initial_world
	   ~name:"ball_in"
           ~width:width
	   ~height:height
	   ~to_draw:draw
	   ~on_tick:on_tick
	   ~on_key_press:key_draw 
	   ~rate:rate         (* ゲームの動く速さ *)
	   ~stop_when:game_finished
	   ~to_draw_last:draw_game_over 
