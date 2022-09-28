Extensions [
  gis   pathdir  csv  profiler table
]
globals[ ;varibles used by all agents
  t                   ;time step - one day
  resolution          ;how much each pixel represents - landscape resolution
  ;---species and landscape parameters
  values              ;specie parameters
  forest              ;values of habitat amount
  clumpiness          ;values of clumpiness
  ;---patchsets
  habitat             ;agentset of habitat patches
  matrix              ;agentset of matrix patches
  edge                ;agentset of edge patches
  available-matrix    ;agentset of matrix patches with specific distance (95-105 cells) from the edge, to initiate individuals, and avoid turtles to be moved to isolated matrix cells within habitat patches.


  movement
  H'_hab

  ;---outputs
  decision_list

  mean_linear_dist_H
  mean_linear_dist_En
  mean_linear_dist_T
  mean_dist_H           ;mean of distance realized per all turtles that settled
  mean_dist_En           ;mean of distance realized per all turtles that settled
  mean_dist_T
  mean_H'_H              ;mean patch quality of all patches where turtles settled
  mean_H'_En              ;mean patch quality of all patches where turtles settled
  mean_H'_T
  report_e_C          ;report the value of e_C per group
  total_set_H         ;total settlers by habitat quality
  total_set_En        ;total settlers by energy
  total_set_T
  mortality           ;mortality rate
  mean_En_end_H
  mean_En_end_En
  mean_En_end_T
  mean_time_set_H
  mean_time_set_En
  mean_time_set_T
  final_occupancy

  location-e_C

  ;--- to calculate cell-occupancy
  c_occupancy
  occup_step

  v
  occupancy_0
  occupancy_05
  occupancy_1
  occupancy_15
  occupancy_2

  ;--- to create agents in all fragments
  id-list             ;list of id values of patches
  N                   ; variable onle to generate turtles in all patches in setup

  ;--- to store distribution - locations
  distribution
  p                   ;run number
  file_dist           ;to create name for distribution file

  all_e_C             ;list of all exponents
  each_group          ;total of individuals per group of behaviour
  group               ;final group of each behaviour - to calculate final outputs
  group1
]
patches-own [
  cover               ; vegetal cover (matrix/pasture or forest)
  patches_ID          ; ID of each patch
  patches_DIST        ; distance (m) of each cell in the matrix from the patch
  Q                   ; Habitat quality of the patch
  ;patches-occupied
  location
]
turtles-own[
 e_C                  ; exponent of settlement behaviour - level of plasticity
 PR                   ; perceptual range
 Ci                   ; initial habitat quality threshold
 C                    ; minimal habitat quality required - threshold
 H'                   ; mean habitat quality within PR per turtle
 status               ; resident (res) or disperser (dis) or settler (set)
 En                   ; energetic individual condition - range 0-1 (maximum)
 patch-q              ; habitat quality of the area within PR where each turtles settled
 my_id                ; ID of initial patch before dispersal
 decision             ; if decision of settle was based on habitat quality or energy
 time_set             ; time to settle

 ;---distance
 initial_patch        ; the initial patch
 total_dist           ; total distance realized before settle
 linear_dist          ; euclidian distance
 dist                 ; temporary distance realized per day
 max_dist             ; maximum distance per day

 ;---movement
 oriented?
 new-xcor
 new-ycor
 ;---------------variables to step lenght - ;Truncated Pareto Distribution
 l                    ; step lenght
 Xmax-no
 Xmin-no
 expo-no
 Xmax-o
 Xmin-o
 expo-o
 ;---------------variables to turning angle
 turning_angle        ; turning angle
 ang                  ;angle + turning angle
 mean-ang-o
 sd-ang-o
 mean-ang-no
 sd-ang-no
]

to setup
  clear-all
  random-seed 10
  set-current-directory output_directory ; Define directory to save output
  set t 0 ; Start time steps at 0

;---------------------------------------------------------------------------------------------------
;--------------------------------------CREATE ENVIRONMENT-------------------------------------------
;---------------------------------------------------------------------------------------------------
  ;resize-world -512 512 -512 512           ; Define landscape 1024x1024.
  ;set-patch-size 0.7
  resize-world -256 256 -256 256           ; Define small landscape just for testing
  resize-world -100 100 -100 100
  set-patch-size 2.5
  set resolution 10                        ; Each pixel/cell is 10m

;--------------------------------Import and define landscape from directory
  imp_def_PATCHES_ONLY
  imp_def_ID_PATCHES
  imp_def_DIST_PATCHES
  landscape_parameters

;--------------------------------Define patches types
  set habitat patches with [cover = 1]
  set matrix patches with [cover = 0]
  set edge patches with [cover = 1 and count neighbors4 with [cover = 1] < 4]
  set available-matrix patches with [cover = 0 and  patches_DIST > perceptual_range]

;--------------------------------Define habitat quality in habitat patches
  create_hab_quality_surface

;---------------------------------------------------------------------------------------------------
;--------------------------------------CREATE AGENTS------------------------------------------------
;---------------------------------------------------------------------------------------------------

;--------------------------------Create turtles in the habitat patches
  create_turtles_all_patches                      ; create individuals in every patch - number of individuals is proportional to patch size.
  ;create_turtles_half_landscape

;--------------------------------Import species parameters
  specie_values                                   ;import inputs values of each species

;--------------------------------Define turtles variables
  ask turtles [
   ;pen-down
   set size 1
   set status "set" set color red
   set PR perceptual_range / resolution           ;100m represent 10 pixels, because each
   set H' mean [(Q)] of patches in-radius PR      ;Mean habitat quality within perceptual range
   set total_dist 0                               ;Total distance realized before settle
   set dist 0                                     ;Temporary distance realized during dispersal per day - reset at every step
   ;set max_dist max_distance_per_day / resolution ;Maximum distance during dispersal per day  -- Need to get this data with Marquinhos!! **
   set max_dist (random-normal dmean dsd) / resolution
   set En 1                                       ;Energy - Body conditions
   set my_id [patches_ID] of patch-here           ;Id of initial patch
   set initial_patch patch-here

   ;set Ci [(Q)] of patch-here
   set Ci 0.5 + random-float 0.4
  ]

;--------------------------------Define settlement behaviors (e_C) and creating turtles sets
  assign_settlement_behaviour

;---------------------------------------------------------------------------------------------------
;------------------------------------- INITIAL OUTPUTS----------------------------------------------
;---------------------------------------------------------------------------------------------------
  set c_occupancy table:make ; create table to store occupanccy
  cell_occupancy ;-------------------------------------------------Calculate Initial Cell Occupancy
  ;store-turtles-location-initial-per-behavior ;--------------------Store initial distribution per settlement behaviour

;----------------------------------------------------------------------------------------------------Move to edge to start dispersal
  ask turtles[
    move-to min-one-of edge [distance myself]                                               ;move individual to the edge
    ;let available-matrix-i available-matrix with [pxcor < -100]                             ;move to edge but avoid to cross to the other half
    ;move-to min-one-of available-matrix-i [distance myself]                                 ;facing the matrix
    ;face min-one-of edge [distance myself]
    ;set heading heading + 180
    move-to one-of neighbors with [cover = 0]
    set status "dis"
  ]
  ;-------------------------------------------------------------------------------------------GENERATE OUTPUTS------------------------------------------------------------------------------------------
   if not file-exists? "output.txt"
    [
    file-open "output.txt"
      file-print (word "run"
                       "," "specie" "," "perceptual_range" "," "landscape_number" "," "habitat_amount" "," "clumpiness"
                        "," "min_en"                                                                     ;minimum energy required
                        "," "e_C"                                                                        ;expoent of decreasing habitat quality requirement with energy
                        "," "decision"
                        "," "total_set_T"                                                                ;total settlers in total
                        "," "mean_linear_dist_T"                                                         ;mean linear distance in total
                        "," "mean_dist_T"                                                                ;mean distance realized in total
                        "," "mean_H_T"                                                                   ;mean habitat quality of settlement patch in total
                        "," "mortality"                                                                  ;mortality rate
                        ;"," "total-patches" ","  "occupied" "," "frag-occupancy"                         ;frag-occupancy variables
                        "," "occupancy"
                        "," "recharge-rate" "," "discharge-rate"                                         ;recharge and discharge rate
                        "," "mortality_rate_dispersal"                                                   ;mortality rate in dispersal
                        "," "mean_En_end_T"                                                              ;mean energetic condition in the end in total
                        "," "mean_time_set_T"                                                            ;mean time to settle in total
      )
      file-close]

  if occupancy_output = TRUE [
  if not file-exists? "occupancy.txt"
  [
    file-open "occupancy.txt"
    file-print c_occupancy
    file-close
  ]
  ]
  reset-ticks
end

to go
  if (ticks = 0) [if count turtles < individuals [stop
      show (word "Simulation for landscape " num_lands " stoped")]] ; stop simulations that had not enough individuals because of lower habitat patches. - for simulations with turtles in half landscape.
  if (ticks < 1) [reset-timer]
  tick
  set t (t + 1)

  ;calculate occupancy and store in output
  cell_occupancy
  if occupancy_output = TRUE [
    file-open "occupancy.txt"
    file-print c_occupancy
    file-close
    ]

  ask turtles [
; --------------------------------------------------------------------- For dispersers - Keep dispersing or Settling - Settlement Choice
  if status = "dis"
     [if En <= 0 [die]                                                ; Mortality by low energetic condition
      set H' mean [(Q)] of patches in-radius PR                       ; Update H'- habitat quality
      set C (Ci * (En) ^ (e_C))                                       ; Update C of turtles

      ifelse [cover] of patch-here = 0                                ; If disperser are still in the matrix...
        [disperse
          discharge]                                             ; keep dispersing
        [ifelse H' >= C or En < min_En                                ; If reached any fragment - Settlement Choice: if habitat quality is higher then requirement, or if energy is too low to keep dispersing.
          [settle]
           ;else
          [recharge                                                   ; If decide not to settle, recharge one time step, which represents the foraging in this fragment, and then...
           disperse]                                                  ; Disperse.
        ]
    ]
  ]

  if path_outputs = TRUE
  [let turtles_path (turtle-set turtle 0 turtle 10 turtle 20 turtle 30 turtle 40 turtle 50 turtle 60 turtle 70 turtle 80 turtle 90)
    ask turtles_path [save_paths]]

  ; Generate outputs
  if count turtles with [status = "dis"] = 0
  [ store-turtles-location-final-per-behavior
    set decision_list list "H" "En"
    set v 0
    foreach all_e_C [ ;FOR EACH GROUP OF BEHAVIOUR, CALCULATE OUTPUTS AND SAVE
        x ->
        set group1 turtles with [e_C = x]
        set mortality precision ((each_group - count group1) / each_group) 2
        set final_occupancy item v c_occupancy
        set v (v + 1)
      foreach decision_list[
        y ->
        set group group1 with [decision = y]
        define-output-variables
        file-open "output.txt"
        file-print (word p
                        "," Specie "," perceptual_range "," num_lands "," forest "," clumpiness        ;Basic variables
                        "," min_en                                                                     ;minimum energy required
                        "," x                                                                          ;expoent of decreasing habitat quality requirement with energy
                        "," y                                                                          ;decision
                        "," total_set_T                                                                ;total settlers in total
                        "," mean_linear_dist_T                                                         ;mean linear distance in total
                        "," mean_dist_T                                                                ;mean distance realized in total
                        "," mean_H'_T                                                                   ;mean habitat quality of settlement patch in total
                        "," mortality                                                                  ;mortality rate
                        "," final_occupancy
                        "," recharge-rate "," discharge-rate                                           ;recharge and discharge rate
                        "," mortality_rate_dispersal                                                   ;mortality rate in dispersal
                        "," mean_En_end_T                                                              ;mean energetic condition in the end in total
                        "," mean_time_set_T                                                            ;mean time to settle in total
        )
        file-close
    ]]
    show (word "Execution finished in "timer" seconds")
    stop]
end

to settle
  if e_C = 0 [set color black]  if e_C = 0.5 [set color red]  if e_C = 1 [set color blue]  if e_C = 1.5 [set color white]  if e_C = 2 [set color pink]
  ;set color black                                             ; Settle and stop.
  set patch-q H'                                             ; register the habitat quality of the settlement area
  set status "set"                                           ; define status as settler
  ifelse H' >= C [set decision "H"][set decision "En"]       ; register setller by energy or by hab quality
  set time_set t                                             ; register time to settlement
  stop
end

to move-to-edge
  set my_id [patches_ID] of patch-here
  move-to min-one-of edge [distance myself]                                             ;move individual to the edge
  move-to min-one-of available-matrix [distance myself]                                 ;facing the matrix
end

to disperse
  set color yellow
  ;set status "dis"
  set new-xcor xcor
  set new-ycor ycor
  while [dist < max_dist]
  [
  ifelse [cover] of patch-here = 0 ;calculate the parameter step lenght and turning angles based on movement in matrix or habitat
    [movement-matrix]
    [movement-habitat]

    if movement_parameters_output = TRUE [save_movement_parameters]

    set new-xcor (new-xcor + (l / resolution * sin(ang)))                                                                ;define new coordinate X
    set new-ycor (new-ycor + (l / resolution * cos(ang)))                                                                ;define new coordenate Y
    set dist (dist + l)                                                                                                  ;update distance realized
    if path_outputs = TRUE [setxy new-xcor new-ycor]                                                                     ;move to new coordinates - just when is needed to save_paths
  ]
  if path_outputs = FALSE [setxy new-xcor new-ycor]                                                                      ;move to new coordinates
  set total_dist total_dist + dist
  set dist 0                                                                                                            ;reset distance
  if random-float 1 < mortality_rate_dispersal                                                                           ;all dispersers faces mortality after moving
  [die]
  if [cover] of patch-here = 1 [set my_id [patches_ID] of patch-here] ; after movement, if individual is in a habitat patch, save patch id. Then, when it reaches the matrix, it avoids to return.
end

to save_movement_parameters
  file-open "movement_parameters.txt"
  file-print (word p "," movement "," l "," turning_angle "," ang)
  file-close
end

to movement-matrix ; calculate l and ang
  set movement "matrix"
 ;ifelse not any? patches in-radius PR with [cover = 1]
  ifelse not any? patches in-radius PR with [cover = 1 and patches_ID != [my_id] of myself]                                        ;if there is no habitat patch within PR
    [set oriented? false
      let RAN random-float 1
      set l ((Xmax-no ^ (expo-no + 1) - Xmin-no ^ (expo-no + 1)) * RAN + Xmin-no ^ (expo-no + 1)) ^ (1 / (expo-no + 1))  ;define step lenght from non-oriented distribution
      set turning_angle random-normal mean-ang-no sd-ang-no                                                              ;define turning angle from non-oriented distribution
      set ang (heading + turning_angle)                                                                                  ;define absolute angle
    ]
    [set oriented? true
      let near-habitat-patch min-one-of patches in-radius PR with [cover = 1 and patches_ID != [my_id] of myself] [distance myself]  ;define the near habitat patch which are not the same as the previous one
      ;let near-habitat-patch min-one-of patches in-radius PR with [cover = 1] [distance myself]                          ;define the near habitat patch
      face near-habitat-patch                                                                                            ;face the near habitat patch
      let RAN random-float 1
      set l ((Xmax-o ^ (expo-o + 1) - Xmin-o ^ (expo-o + 1)) * RAN + Xmin-o ^ (expo-o + 1)) ^ (1 / (expo-o + 1))         ;define step lenght from oriented distribution
      set turning_angle random-normal mean-ang-o sd-ang-o                                                                ;define turning angle from oriented distribution
      set ang (heading + turning_angle)                                                                                  ;define absolute angle
    ]
end

to movement-habitat
  set movement "habitat"
  let RAN random-float 1
  ;set l ((Xmax-no ^ (expo-no + 1) - Xmin-no ^ (expo-no + 1)) * RAN + Xmin-no ^ (expo-no + 1)) ^ (1 / (expo-no + 1))
  set l ((10 ^ (-2 + 1) - 2 ^ (-2 + 1)) * RAN + 2 ^ (-2 + 1)) ^ (1 / (-2 + 1))
  set turning_angle random-normal 0 90
  set ang (heading + turning_angle)
end

to discharge
  if En > 0 [
   set En (En - (discharge-rate))
  ]
end

to recharge
  if En <= 1 [
    set En (En - (recharge-rate * ([Q] of patch-here)))
  ]
end

to define-output-variables
  ifelse count group = 0
  [set total_set_T 0
    set mean_H'_T 0
    set mean_dist_T 0
    set mean_linear_dist_T 0
    set mean_En_end_T 0
    set mean_time_set_T 0
  ]
  [
  ; TOTAL SETTLERS
  set total_set_T count group with [status = "set"]    ;total settlers

  ;------- Mean Habitat Quality of Settlement Area
  let H'-list_T [H'] of group with [status = "set"]
  set mean_H'_T precision (mean H'-list_T) 2

  ;------- Mean Distance Realized by Settlers
  let dist-list_T [total_dist] of group with [status = "set"]
  set mean_dist_T precision (mean dist-list_T) 2

  ;------- Mean Linear Distance
  ask turtles [set linear_dist ((distance initial_patch) * resolution)]

  let l_dist-list_T [linear_dist] of group with [status = "set"]
  set mean_linear_dist_T precision (mean l_dist-list_T) 2

   ;------- Mean Energy Condition in the end
  let list_En_end_T [En] of group with [status = "set"]
  set mean_En_end_T precision (mean list_En_end_T) 2

  ;------- Mean Time to Settle
  let list_time_set_T [time_set] of group with [status = "set"]
  set mean_time_set_T precision (mean list_time_set_T) 2
  ]
end



to store-turtles-location-initial-per-behavior
foreach all_e_C[
  x ->
  set distribution nobody
  ask habitat [ifelse count turtles-here with [e_C = x] > 0 [set location x][set location -1]]
  ask matrix [set location "NA"]
  ask one-of patches [set distribution gis:patch-dataset location]
  set p 0
  set file_dist word "distribution_" x
  while [file-exists? (word p file_dist "_" "i" ".asc")]
  [set p (p + 1)]
  gis:store-dataset distribution (word p file_dist "_" "i" ".asc" )
  ]
end

to store-turtles-location-final-per-behavior
foreach all_e_C[
  x ->
  set distribution nobody
  ask habitat [ifelse count turtles-here with [e_C = x] > 0 [set location x][set location -1]]
  ask matrix [set location "NA"]
  ask one-of patches [set distribution gis:patch-dataset location]
  ;set p 0
  set file_dist word "distribution_" x
  ;while [file-exists? (word p file_dist "_" "i" ".asc")]
  ;[set p (p + 1)]
  gis:store-dataset distribution (word p file_dist "_" "f" ".asc" )
  ]
end

to save_paths
  file-open "paths.txt"
  file-print (word p "," who "," t "," xcor "," ycor "," [cover] of patch-here)
  file-close
end

;---------------------------------------------------------------------------------------------------
;--------------------------------------SUB-MODEL IN SETUP-------------------------------------------
;---------------------------------------------------------------------------------------------------

; CREATE ENVIROMENT

to imp_def_PATCHES_ONLY ;----------------------------------LANDSCAPE WITH ONLY PATCHES
  ;------------------------------------------------------Import landscape
  let directory word landscape_directory "exported_ascii_MS_HABMAT_PATCHES_ONLY/" ;define directory
  let direct_list sort pathdir:list directory ;list of files in directory in order
  let landscape item num_lands direct_list ;get one of the landscape from the list based on num-lands and define as landscape
  let dir_landscape word directory landscape ;set the new_landscape with the directory to import
  ;------------------------------------------------------Define landscape
  let landscape-dataset gis:load-dataset dir_landscape
  gis:set-world-envelope-ds gis:envelope-of landscape-dataset ;define the world size similar to the landscape imported
  gis:apply-raster landscape-dataset cover ;get values of the landscape to variable cover

end

to imp_def_ID_PATCHES ;-----------------------LANDSCAPE WITH ONLY PATCHES - ID
  ;------------------------------------------------------Import landscape ID_PATCHES
  let directory_ID_PATCHES word landscape_directory "exported_ascii_MS_HABMAT_PATCHES_ONLY_PID/" ;define directory
  let direct_list_ID_PATCHES sort pathdir:list directory_ID_PATCHES;list of files in directory
  let landscape_ID_PATCHES item num_lands direct_list_ID_PATCHES ;get one of the landscape from the list based on num-lands and define as landscape_ID
  let dir_landscape_ID_PATCHES word directory_ID_PATCHES landscape_ID_PATCHES
  ;------------------------------------------------------Define landscape ID_PATCHES
  let landscape-ID_PATCHES-dataset gis:load-dataset dir_landscape_ID_PATCHES
  gis:set-world-envelope-ds gis:envelope-of landscape-ID_PATCHES-dataset ;define the world size similar to the landscape imported
  gis:apply-raster landscape-ID_PATCHES-dataset patches_ID ;get values of the landscape to variable cover
end

to imp_def_DIST_PATCHES ;-----------------------LANDSCAPE  WITH ONLY PATCHES - DIST
  ;------------------------------------------------------Import landscape PATCHES_DIST
  let directory_DIST_PATCHES word landscape_directory "exported_ascii_MS_HABMAT_PATCHES_ONLY_DIST/" ;define directory
  let direct_list_DIST_PATCHES sort pathdir:list directory_DIST_PATCHES ;list of files in directory
  let landscape_DIST_PATCHES item num_lands direct_list_DIST_PATCHES  ;get one of the landscape from the list based on num-lands and define as landscape_DIST
  let dir_landscape_DIST_PATCHES word directory_DIST_PATCHES landscape_DIST_PATCHES
  ;------------------------------------------------------Define landscape PATCHES_DIST
  let landscape-DIST_PATCHES-dataset gis:load-dataset dir_landscape_DIST_PATCHES
  gis:set-world-envelope gis:envelope-of landscape-DIST_PATCHES-dataset ;define the world size similar to the landscape imported
  gis:apply-raster landscape-DIST_PATCHES-dataset patches_DIST ;get values of the landscape to variable cover
end

to landscape_parameters ;import landscape parameter for each landscape - habitat amount and clumpiness
  let imp_land_par (csv:from-file word data_directory "/landscape_parameters.csv")
  let land_values item (num_lands + 1) imp_land_par
  set forest item 1 land_values
  set clumpiness item 2 land_values
end

to create_hab_quality_surface ;Atkins et al. 2019
  ;ask habitat[set Q 0.5
  ;            set Q Q + random-float 0.5] ;;set quality values for habitat patches
  ;ask habitat[set Q Q + random-float ns - (2 * ns)] ;;add noise to landscape by randomly increasing or decreasing patch quality in habitat
  ;ask habitat[set pcolor scale-color green Q 0 1]
  ;ask matrix[set Q 0 set pcolor brown]
  foreach remove-duplicates [patches_ID] of habitat[
    x -> ask one-of habitat with [patches_ID = x][set Q 0.5 + random-float 0.4]
  ]
  repeat (count habitat - length remove-duplicates [patches_ID] of habitat)[ask one-of habitat [assign]]
  ask habitat[set pcolor scale-color green Q 0.5 1]
  ask matrix[set Q 0 set pcolor brown]
end

to assign
 ifelse random-float 1.1 < ac
  [let model habitat with [Q != 0 and count neighbors with [Q = 0] > 0]
    if any? model [
      ask one-of model [
        ask neighbors with [Q = 0] [set Q ([Q] of myself) + random-float 0.01]
      ]
    ]
  ]
  [let cand habitat with [Q = 0]
    if any? cand [
      ask one-of cand [
        set Q 0.5 + random-float 0.4]
    ]
  ]
end

; CREATE AGENTS

to create_turtles_half_landscape
  let half_habitat habitat with [pxcor > -50 and pxcor < 50]
  ;let half_habitat matrix with [pxcor < -100 and patches_DIST > 200]
  if any? half_habitat [
  ask n-of individuals half_habitat [sprout 1]
  ]
  if count turtles < individuals [stop]
end

to create_turtles_all_patches ; create individuals in every patch - number of individuals is proportional to patch size.
  set id-list []
  set id-list sort remove-duplicates [patches_ID] of habitat
  set N 0
  while [N < individuals][
    foreach id-list
  [i ->
      let pat habitat with [patches_ID = i] let y count pat  let size-prop (y / (count habitat))
      let nn (size-prop * individuals)
      if nn < 1 [set nn 1]
      ask n-of nn habitat with [patches_ID = i and not any? turtles-here] [ sprout 1 ]
      set N (N + nn)]
  ]
  ifelse count turtles < individuals
  [let rest (individuals - count turtles)
    ask n-of rest habitat with [not any? turtles-here] [sprout 1]]
  [let rest (count turtles - individuals)
    ask n-of rest turtles [die]]
end

to specie_values ;import parameters for each specie!
  let imp_values (csv:from-file word data_directory "/species_values.csv")
  if Specie = "DA" [set values item 1 imp_values]
  if Specie = "PQ" [set values item 2 imp_values]
  if Specie = "MP" [set values item 3 imp_values]
  ask turtles [
  set Xmax-no item 1 values
  set Xmin-no item 2 values
  set expo-no item 3 values
  set mean-ang-no item 4 values
  set sd-ang-no item 5 values
  set Xmax-o item 6 values
  set Xmin-o item 7 values
  set expo-o item 8 values
  set mean-ang-o item 9 values
  set sd-ang-o item 10 values
  ]
end

to assign_settlement_behaviour
  set all_e_C [] ;empty list
  let all_e_Ci (range initial (ends + interval) interval) ;new list with values of e_C for each behaviour
  foreach all_e_Ci[ ;edit list to avoid float problems, then define only one decimal.
    x -> let a precision x 1
    set all_e_C lput a all_e_C
  ]
  set each_group individuals / length all_e_C ;create turtle set based on settlement behaviours by calculate the total of behaviour and divide the number of individual for that.
  ask turtles [set e_C "NA"] ;then, assign e_C per group
  foreach all_e_C[
    x -> ask n-of each_group turtles with [e_C = "NA"] [set e_C x ]
  ]
end

;---------------------------------------------------------------------------------------------------
;----------------------------------------------OUTPUTS----------------------------------------------
;---------------------------------------------------------------------------------------------------

to cell_occupancy
  ;set c_occupancy []
  ;foreach all_e_C[
  ;x ->
  ;  let occup count habitat with [count turtles-here with [e_C = x] > 0]
  ;  set c_occupancy lput occup c_occupancy
  ;]

  foreach all_e_C[
  x -> set occup_step count habitat with [count turtles-here with [status = "set" and e_C = x] > 0]
    table:put c_occupancy x occup_step
  ]

end
@#$#@#$#@
GRAPHICS-WINDOW
489
87
999
598
-1
-1
2.5
1
10
1
1
1
0
1
1
1
-100
100
-100
100
1
1
1
ticks
30.0

INPUTBOX
29
439
444
515
landscape_directory
/home/kekuntu/Documents/phd_project/Chapter_2/Landscapes/100land_10res/landscape_100land_10res/
1
0
String

INPUTBOX
353
45
420
105
num_lands
0.0
1
0
Number

INPUTBOX
29
595
443
655
output_directory
/home/kekuntu/Documents/phd_project/Chapter_2/output
1
0
String

BUTTON
515
23
608
78
Setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
612
23
702
78
Go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
425
47
483
109
Landscape ID from database (0 to 99)
10
0.0
1

SLIDER
29
128
225
161
individuals
individuals
0
1000
1000.0
100
1
NIL
HORIZONTAL

CHOOSER
113
52
205
97
Specie
Specie
"DA" "PQ" "MP"
0

SLIDER
29
166
225
199
perceptual_range
perceptual_range
0
1000
200.0
50
1
NIL
HORIZONTAL

PLOT
1028
137
1219
287
Population
NIL
NIL
0.0
0.0
0.0
500.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if t > 1 [plot count turtles]"

SLIDER
234
129
431
162
mortality_rate_dispersal
mortality_rate_dispersal
0
0.05
0.01
0.01
1
NIL
HORIZONTAL

PLOT
1028
303
1692
423
Settlers
NIL
NIL
0.0
100.0
0.0
100.0
true
true
"" ""
PENS
"Hab" 1.0 0 -2674135 true "" "if t > 1 [plot count turtles with [decision = \"H\"]]"
"Energy" 1.0 0 -14454117 true "" "if t > 1 [plot count turtles with [decision = \"En\"]]"

INPUTBOX
217
45
345
105
max_distance_per_day
1000.0
1
0
Number

INPUTBOX
29
524
444
584
data_directory
/home/kekuntu/Documents/phd_project/Chapter_2/data
1
0
String

INPUTBOX
34
46
101
106
time_steps
0.0
1
0
Number

INPUTBOX
30
237
119
299
discharge-rate
0.01
1
0
Number

INPUTBOX
121
237
208
299
recharge-rate
0.005
1
0
Number

INPUTBOX
212
237
274
299
min_en
0.3
1
0
Number

PLOT
1029
553
1693
681
Mean Habitat Threshold of Dispersers with Plastic Behaviour
NIL
NIL
0.0
100.0
0.0
1.0
false
true
"" ""
PENS
"D 0.0" 1.0 0 -955883 true "" "if t > 1 and turtles with [status = \"dis\"] != 0 [plot mean [(C)] of turtles with [status = \"dis\" and e_C = 0.0]]"
"D 0.5" 1.0 0 -2674135 true "" "if t > 1 and turtles with [status = \"dis\"] != 0 [plot mean [(C)] of turtles with [status = \"dis\" and e_C = 0.5]]"
"D 1.0" 1.0 0 -12087248 true "" "if t > 1 and turtles with [status = \"dis\"] != 0 [plot mean [(C)] of turtles with [status = \"dis\" and e_C = 1.0]]"
"D 1.5" 1.0 0 -14454117 true "" "if t > 1 and turtles with [status = \"dis\"] != 0 [plot mean [(C)] of turtles with [status = \"dis\" and e_C = 1.5]]"
"D 2.0" 1.0 0 -8630108 true "" "if t > 1 and turtles with [status = \"dis\"] != 0 [plot mean [(C)] of turtles with [status = \"dis\" and e_C = 2.0]]"

INPUTBOX
30
336
89
396
initial
0.0
1
0
Number

TEXTBOX
40
211
190
229
Energetic dynamics
12
0.0
1

TEXTBOX
181
10
254
34
INPUTS 
20
0.0
1

TEXTBOX
33
312
231
342
Habitat Selection Behaviors
12
0.0
1

TEXTBOX
34
415
384
433
Directories (landscapes, input data, and output)\n
12
0.0
1

INPUTBOX
283
237
347
297
ac
0.8
1
0
Number

TEXTBOX
281
182
386
234
Autocorrelation\n    of Habitat \n     Quality \n      (0-1)
10
0.0
1

INPUTBOX
96
336
150
396
interval
0.5
1
0
Number

INPUTBOX
160
336
216
396
ends
2.0
1
0
Number

PLOT
1028
424
1692
551
Dispersers
NIL
NIL
0.0
100.0
0.0
0.0
true
true
"" ""
PENS
"D 0.0" 1.0 0 -3844592 true "" "plot count turtles with [status = \"dis\" and e_C = 0.0]"
"D 0.5" 1.0 0 -2674135 true "" "plot count turtles with [status = \"dis\" and e_C = 0.5]"
"D 1.0" 1.0 0 -12087248 true "" "plot count turtles with [status = \"dis\" and e_C = 1.0]"
"D 1.5" 1.0 0 -14454117 true "" "plot count turtles with [status = \"dis\" and e_C = 1.5]"
"D 2.0" 1.0 0 -8630108 true "" "plot count turtles with [status = \"dis\" and e_C = 2.0]"

PLOT
1200
684
1865
814
Mean Habitat Quality in Settlement
NIL
NIL
0.0
100.0
0.0
0.5
true
true
"" ""
PENS
"Hab" 1.0 0 -5298144 true "" "if t > 1 and turtles with [decision = \"H\"] != 0 [plot mean [(H')] of turtles with [decision = \"H\"]]"
"Energy" 1.0 0 -14070903 true "" "if t > 1 and turtles with [decision = \"En\"] != 0 [plot mean [(H')] of turtles with [decision = \"En\"]]"

PLOT
1224
137
1416
287
Mean Energetic Condition of Dispersers
NIL
NIL
0.0
100.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if t > 1 [ask one-of turtles [plot mean [(En)] of turtles with [status = \"dis\"]]]\n"

SWITCH
27
668
305
701
movement_parameters_output
movement_parameters_output
1
1
-1000

SWITCH
26
704
215
737
occupancy_output
occupancy_output
0
1
-1000

SWITCH
223
704
377
737
path_outputs
path_outputs
1
1
-1000

PLOT
662
615
861
765
Histogram Q
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"  set-plot-x-range min [Q] of habitat max [Q] of habitat\n  ;set-plot-y-range 0 (count habitat / 2)\n  set-histogram-num-bars 10\n  set-plot-pen-mode 1" "\n"
PENS
"default" 1.0 0 -16777216 true "" "histogram [Q] of habitat"

PLOT
457
615
657
765
Max Dist per Day
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"  set-plot-x-range min [max_dist] of turtles max [max_dist] of turtles\n  ;set-plot-y-range 0 (count habitat / 2)\n  set-histogram-num-bars 10\n  set-plot-pen-mode 1" ""
PENS
"default" 1.0 0 -16777216 true "" "histogram [max_dist] of turtles"

INPUTBOX
241
337
314
397
dmean
1000.0
1
0
Number

INPUTBOX
322
338
390
398
dsd
100.0
1
0
Number

PLOT
1027
10
1557
135
Occupancytable
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"0.0" 1.0 0 -16777216 true "" "plot table:get c_occupancy 0"
"0.5" 1.0 0 -7500403 true "" "plot table:get c_occupancy 0.5"
"1.0" 1.0 0 -2674135 true "" "plot table:get c_occupancy 1"
"1.5" 1.0 0 -955883 true "" "plot table:get c_occupancy 1.5"
"2.0" 1.0 0 -6459832 true "" "plot table:get c_occupancy 2"

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment_linux" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="data_directory">
      <value value="&quot;/home/kekuntu/Documents/phd_project/Chapter_2/data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output_directory">
      <value value="&quot;/home/kekuntu/Documents/phd_project/Chapter_2/output&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="landscape_directory">
      <value value="&quot;/home/kekuntu/Documents/phd_project/Chapter_2/100land_10res/landscape_100land_10res/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mortality_rate_dispersal">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perceptual_range">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Specie">
      <value value="&quot;DA&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="num_lands" first="0" step="1" last="4"/>
    <enumeratedValueSet variable="individuals">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_distance_per_day">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="discharge-rate">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recharge-rate">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_en">
      <value value="0.1"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="interval">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ends">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ns">
      <value value="0.09"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement_parameters_output">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="occupancy_output">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="path_outputs">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
