Extensions [
  gis   pathdir  csv  profiler table r palette
]
globals[ ;varibles used by all agents
  p                   ;run number
  t                   ;time step - one day
  resolution          ;how much each pixel represents - landscape resolution
  landscape           ;landscape file being used
  one-frag?           ;check if the landscape has only one ID patch.

  ;---species and landscape parameters
  values              ;specie parameters
  forest              ;values of habitat amount
  clumpiness          ;values of clumpiness

  ;---patchsets
  habitat             ;agentset of habitat patches
  matrix              ;agentset of matrix patches
  edge                ;agentset of edge patches
  available-matrix    ;matrix away to initial patch to start dispersing

  ;---list of colors per turtle
  list-colors         ;code of colors per group of turtles

  ;--- to calculate cell-occupancy
  c_occupancy
  occup_step

  ;--- to create agents in all fragments
  id-list             ;list of id values of patches
  N                   ; variable onle to generate turtles in all patches in setup

  ;--- to store distribution - locations
  distribution
  w

  ;--- define behaviour and turtle sets
  all_e_C             ;list of all exponents
  each_group          ;total of individuals per group of behaviour

  ;--- turtle set of individuals to storage trajectories
  turtle_path

  ;--- count mortality
  mort_pred ;mortality by predation during dispersal
  mort_ener ;mortality by lack of energy - starvation
]
patches-own [
  cover               ; vegetal cover (matrix/pasture or forest)
  patches_ID          ; ID of each patch
  patches_DIST        ; distance (m) of each cell in the matrix from the patch
  Q                   ; Habitat quality of the patch
  location            ; Storage distribution
  ;ID                  ; ID calculated
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
 initial_id           ; ID of initial patch before dispersal
 final_id
 list_previous_id     ; list of ID of fragments visited
 decision             ; if decision of settle was based on habitat quality or energy
 time_set             ; time to settle
 min_En               ; minimal energey to keep dispersing
 death                ; decision to die in movement in matrix or habitat

 ;---distance
 initial_patch        ; the initial patch
 total_dist           ; total distance realized before settle
 linear_dist          ; euclidian distance
 dist                 ; temporary distance realized per day
 max_dist             ; maximum distance per day

 ;---movement
 movement             ; movement in matrix or habitat
 oriented?            ; movement oriented in matrix
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
  reset-timer
  random-seed 10
  set-current-directory output_directory ; Define directory to save output
  set t 0 ; Start time steps at 0

;---------------------------------------------------------------------------------------------------
;--------------------------------------CREATE ENVIRONMENT-------------------------------------------
;---------------------------------------------------------------------------------------------------
  resize-world (world-size - (world-size * 2)) world-size (world-size - (world-size * 2)) world-size
  set-patch-size patch-sizes
  set resolution patch-resolution                   ; Each pixel/cell is 10m

;--------------------------------Import and define landscape from directory
  imp_def_PATCHES_ONLY
  imp_def_ID_PATCHES
  imp_def_DIST_PATCHES
  landscape_parameters

;--------------------------------Define patches types
  set habitat patches with [cover = 1]
  set matrix patches with [cover = 0]
  set edge habitat with [count neighbors4 with [cover = 1] < 4]
  set available-matrix patches with [cover = 0 and  patches_DIST > perceptual_range]
  ;if length (remove-duplicates [patches_ID] of habitat) > 1 [set one-frag? true] ;check if landscape has only one frag.

;--------------------------------Calculate new ID - patches_ID in fragmented landscapes with high habitat amount usually becames one big fragment.
  ;ask patches [set ID 0]
  ;define_patches_ID

;--------------------------------Define habitat quality in habitat patches
  ifelse import_hab_quality_surface = true
  [import_Q]
  [ifelse hab_quality_per_frag = true
    [create_hab_quality_surface_per_frag]
    [ifelse ac_simple = true
      [create_hab_quality_surface_simple]
      [create_hab_quality_surface]
    ]
  ]

  color-patches

;---------------------------------------------------------------------------------------------------
;--------------------------------------CREATE AGENTS------------------------------------------------
;---------------------------------------------------------------------------------------------------

;--------------------------------Create turtles in the habitat patches
  create_turtles_all_patches                      ;create individuals in every patch - number of individuals is proportional to patch size.
  ;create_turtles_half_landscape

;--------------------------------Import species parameters
  specie_values                                   ;import inputs values of each species

;--------------------------------Define turtles variables
  ask turtles [
   ;pen-down
   set size 1.5
   set status "dis"
   set PR perceptual_range / resolution           ;100m represent 10 pixels, because each
   set H' mean [(Q)] of patches in-radius PR      ;Mean habitat quality within perceptual range
   set total_dist 0                               ;Total distance moved before settle
   set dist 0                                     ;Temporary distance realized during dispersal per day - reset at every step
   set max_dist (random-normal dmean dsd) / resolution ;Maximum distance during dispersal per day  -- Need to get this data with Marquinhos!! **
   set En 1                                       ;Energy - Body conditions
   set min_En (random-normal En_mean En_sd)       ;Minimal energy to keep dispersing
   set initial_id [patches_ID] of patch-here      ;Id of initial patch
   set list_previous_id []                        ;start list of visited fragments
   set list_previous_id lput initial_id list_previous_id
   set initial_patch patch-here

   ifelse origin_site = true
    [set Ci [(Q)] of patch-here]
    [set Ci min_Ci + random-float var_Ci]

    let goal min-one-of available-matrix [distance myself] face goal move-to goal ;move to matrix.
  ]

;--------------------------------Define settlement behaviors (e_C) and creating turtles sets e colors
  assign_settlement_behaviour

;--------------------------------Calculate occupancy
  set c_occupancy table:make ; create table to store occupanccy
  cell_occupancy ;Calculate Initial Cell Occupancy

  if path_outputs = TRUE [turtle_path_set] ; define turtle set to store trajectories

  set mort_pred 0
  set mort_ener 0
;---------------------------------------------------------------------------------------------------
;------------------------------------- INITIAL OUTPUTS----------------------------------------------
;---------------------------------------------------------------------------------------------------
   save_sim_parameters

  if occupancy_output = TRUE [ ;CHECAR!
  if not file-exists? "occupancy.txt"
  [
    file-open "occupancy.txt"
    file-print (word "run" "," "t" "," "occupancy")
    file-print (word p "," t "," c_occupancy)
    file-close
  ]
  ]

  create_output
  reset-ticks
  show (word "Setup timer: " timer " seconds -> Simulation landscape " num_lands " running...")
end

to go
  ;if (ticks = 0) [if count turtles < individuals [stop
  ;    show (word "Simulation for landscape " num_lands " stoped")]] ; stop simulations that had not enough individuals because of lower habitat patches. - for simulations with turtles in half landscape.
  reset-timer
  reset-ticks
  ;show t - use this for headless simulations
  tick
  set t (t + 1)

  ;calculate occupancy and store in output
  cell_occupancy
  if occupancy_output = TRUE [
    file-open "occupancy.txt"
    file-print (word p "," t "," c_occupancy)
    file-close
    ]

  ask turtles [
    if status = "dis" [
      if En <= 0 [set mort_ener (mort_ener + 1) die] ; Mortality by low energetic condition
      update_variables

      ifelse t < min_step_before_settle
      [disperse]
      [;ifelse one-frag? [;if landscape has more than one fragment, individual will avoid  settle on initial id fragment.
        if [cover] of patch-here = 1 [decide_settle]
        ;][if [cover] of patch-here = 1 and [patches_ID] of patch-here != initial_id [decide_settle]]
        if status = "dis" [disperse]
      ] ;if not settle, still dispersing.
    ]
  ]

  ; Generate outputs
  if count turtles with [status = "dis"] = 0 [
    ask turtles
    [
      generate_output
    ]
    ask turtles [set size 5]
    export-view (word "run_" p "_landscape_" num_lands "_view.png")
    show (word "Run number " p ":" " landscape " num_lands " finished in "timer" seconds")
    stop]
end


;---------------------------------------------------------------------------------------------------
;--------------------------------------SUB-MODEL IN GO -------------------------------------------
;---------------------------------------------------------------------------------------------------

to update_variables
  set H' mean [(Q)] of patches in-radius PR                       ; Update H'- habitat quality
  set C (Ci * (En) ^ (e_C))                                       ; Update C of turtles
 ; avoid only the two previous patches.
 ; if length list_previous_ID >= 3 [set list_previous_id (list item 0 list_previous_id)]
end

to movement-matrix ; calculate l and ang
  set movement "matrix"
  ifelse not any? patches in-radius PR with [cover = 1 and not member? patches_ID [list_previous_ID] of myself]       ;if there is no habitat patch within PR
    [set oriented? false
      let RAN random-float 1
      set l ((Xmax-no ^ (expo-no + 1) - Xmin-no ^ (expo-no + 1)) * RAN + Xmin-no ^ (expo-no + 1)) ^ (1 / (expo-no + 1))  ;define step lenght from non-oriented distribution
      set turning_angle random-normal mean-ang-no sd-ang-no                                                              ;define turning angle from non-oriented distribution
      set ang (heading + turning_angle)                                                                                  ;define absolute angle
    ]
    [set oriented? true
      let near-habitat-patch min-one-of patches in-radius PR with [cover = 1 and not member? patches_ID [list_previous_ID] of myself] [distance myself]  ;define the near habitat patch which are not the same as the previous one
      face near-habitat-patch                                                                                            ;face the near habitat patch
      let RAN random-float 1
      set l ((Xmax-o ^ (expo-o + 1) - Xmin-o ^ (expo-o + 1)) * RAN + Xmin-o ^ (expo-o + 1)) ^ (1 / (expo-o + 1))         ;define step lenght from oriented distribution
      set turning_angle random-normal mean-ang-o sd-ang-o                                                                ;define turning angle from oriented distribution
      set ang (heading + turning_angle)                                                                                  ;define absolute angle
    ]

  ; mortality
    if random-float 1 < (mortality_matrix / 10) [set death true]                      ;all dispersers faces mortality after moving
end

to movement-habitat
  let good-place habitat in-radius PR with [Q > [C] of myself]
  if any? good-place [face one-of good-place]
  set movement "habitat"
  let RAN random-float 1
  ;set l ((Xmax-no ^ (expo-no + 1) - Xmin-no ^ (expo-no + 1)) * RAN + Xmin-no ^ (expo-no + 1)) ^ (1 / (expo-no + 1))
  set l ((10 ^ (-2 + 1) - 2 ^ (-2 + 1)) * RAN + 2 ^ (-2 + 1)) ^ (1 / (-2 + 1))
  set turning_angle random-normal 0 90
  set ang (heading + turning_angle)

  ; mortality
  if random-float 1 < (mortality_habitat / 10) [set death true]                      ;all dispersers faces mortality after moving

  let my_id [patches_ID] of patch-here
  if not member? my_id list_previous_id [
    ifelse length list_previous_id > 1 [set list_previous_id replace-item 1 list_previous_id my_id]
    [set list_previous_id lput my_id list_previous_id]
  ]
  ;after movement, if individual is in a habitat patch, save patch id. Then, when it reaches the matrix, it avoids to return.
end

to disperse
  set new-xcor xcor                                                      ;define actual coordinates
  set new-ycor ycor                                                      ;define actual coordinates
  while [dist < max_dist]                                                ;calculate jump per day
  [
  ifelse [cover] of patch-here = 0                                       ;calculate the parameter step lenght and turning angles based on movement in matrix or habitat
    [movement-matrix]
    [movement-habitat]

    if movement_parameters_output = TRUE [save_movement_parameters]

    set new-xcor (new-xcor + (l / resolution * sin(ang)))                ;define new coordinate X
    set new-ycor (new-ycor + (l / resolution * cos(ang)))                ;define new coordinate Y
    set dist (dist + l)                                                  ;update distance moved per day
    if path_outputs = TRUE [save_paths]                                  ;move to new coordinates - just when is needed to save_paths because the individual will perform each movement step, and not only jumps
  ]
  setxy new-xcor new-ycor
  set total_dist total_dist + dist                                       ;calculate total distance moved
  set dist 0                                                             ;reset distance

  if death = true [set mort_pred mort_pred + 1
                   die]                                                  ;all dispersers faces mortality after moving

  discharge                                                              ;individual spend energy while moving
end

to decide_settle
  ;if any? turtles-on patch-here [move-to one-of neighbors] ;avoid two turtles in the same patch
  if H' >= C or En < min_En [                                ; Settlement Choice: if habitat quality is higher then requirement, or if energy is too low to keep dispersing.
  set patch-q H'                                             ; register the habitat quality of the settlement area
  set linear_dist ((distance initial_patch) * resolution)    ; calculate euclidian distance
  set final_id [patches_ID] of patch-here                    ; define ID of settlement patch
  set status "set"                                           ; define status as settler
  ifelse H' >= C [set decision "H"][set decision "En"]       ; register setller by energy or by hab quality
  set time_set t                                             ; register time to settlement
  stop
  ]
end

to discharge ; implement discharge different in habitat and in matrix.
  if En > 0 [
   let patch_type [cover] of patch-here + 1
   set En (En - (discharge-rate / patch_type))  ;energy expenditure will be divided by 2 when in the habitat, and by 1 when in the matrix, by that the discharge will be double in the matrix in comparison with habitat.
  ]
end

;---------------------------------------------------------------------------------------------------
;--------------------------------------SUB-MODEL IN SETUP-------------------------------------------
;---------------------------------------------------------------------------------------------------

; CREATE ENVIROMENT

to imp_def_PATCHES_ONLY ;----------------------------------LANDSCAPE WITH ONLY PATCHES
  ;------------------------------------------------------Import landscape
  let directory word landscape_directory "exported_ascii_MS_HABMAT_PATCHES_ONLY/" ;define directory
  let direct_list sort pathdir:list directory ;list of files in directory in order
  set landscape item num_lands direct_list ;get one of the landscape from the list based on num-lands and define as landscape
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

;to define_patches_ID
;  while [any? habitat with [ID = 0]][
;
;    ask one-of habitat with [ID = 0]
;    [set ID random 50
;      ask neighbors with [cover = 1 and ID = 0][set ID [ID] of myself]
;    ]
;
;    while [any? habitat with [ID = 0 and count neighbors with [cover = 1 and ID != 0] >= 1]]
;    [ask habitat with [ID = 0 and count neighbors with [cover = 1 and ID != 0] >= 1]
;      [set ID [ID] of one-of neighbors with [cover = 1 and ID != 0]
;        ask neighbors with [cover = 1 and ID = 0][set ID [ID] of myself]]
;    ]
;  ]
;end

to landscape_parameters ;import landscape parameter for each landscape - habitat amount and clumpiness
  let imp_land_par (csv:from-file word data_directory "/landscape_parameters.csv")
  let land_values item (num_lands + 1) imp_land_par
  set forest item 1 land_values
  set clumpiness item 2 land_values
end

; HABITAT QUALITY SURFACE

to import_Q
   ;------------------------------------------------------Import landscape
  let directory_Q word landscape_directory hab_quality_folder;define directory
  let direct_list_Q sort pathdir:list directory_Q ;list of files in directory in order
  let surface item num_lands direct_list_Q ;get one of the landscape from the list based on num-lands and define as landscape
  let dir_landscape_Q word directory_Q surface ;set the new_landscape with the directory to import
  show direct_list_Q
  ;------------------------------------------------------Define landscape
  let Q-dataset gis:load-dataset dir_landscape_Q
  gis:set-world-envelope-ds gis:envelope-of Q-dataset ;define the world size similar to the landscape imported
  gis:apply-raster Q-dataset Q ;get values of the landscape to variable hab quality (Q)
end

to create_hab_quality_surface_per_frag
  let j remove-duplicates [patches_ID] of habitat
  foreach j [
   x -> let value 0.5 + random-float 0.4
    ask habitat with [patches_ID = x][set Q value]
  ]
end

to create_hab_quality_surface
  let j remove-duplicates [patches_ID] of habitat
  foreach j [
    x -> ask one-of habitat with [patches_ID = x][set Q 0.5 + random-float 0.4]
  ]
  ask habitat [assign]
end

to assign
 ifelse random-float 1.1 < ac
  [let model habitat with [Q != 0 and count neighbors with [cover = 1 and Q = 0] > 0]
    if any? model [
      ask one-of model [
        ask neighbors with [cover = 1 and Q = 0] [set Q ([Q] of myself) + random-float 0.01]
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

to create_hab_quality_surface_simple
  let list-ac table:make
  table:put list-ac 1 8
  table:put list-ac 2 7
  table:put list-ac 3 6
  table:put list-ac 4 5
  table:put list-ac 5 4
  table:put list-ac 6 3
  table:put list-ac 7 2
  table:put list-ac 8 1

  while [any? habitat with [Q = 0]][

    ask one-of habitat with [Q = 0]
    [set Q 0.5 + random-float 0.4
      ask neighbors with [cover = 1 and Q = 0][set Q [Q] of myself]
    ]

    while [any? habitat with [Q = 0 and count neighbors with [cover = 1 and Q != 0] >= table:get list-ac autocorrelation]]
    [ask habitat with [Q = 0 and count neighbors with [cover = 1 and Q != 0] >= table:get list-ac autocorrelation]
      [set Q [Q] of one-of neighbors with [cover = 1 and Q != 0]
        ask neighbors with [cover = 1 and Q = 0][set Q [Q] of myself]]
    ]
  ]
end

to color-patches
  let minq min [Q] of habitat
  let maxq max [Q] of habitat
  ;ask habitat[set pcolor scale-color green Q minq maxq]
  ask habitat[set pcolor palette:scale-scheme "Sequential" "Greens" 8 Q (minq) (maxq + 0.4)]
  ask matrix[set Q 0 set pcolor brown]
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
  turtle-colors
  set all_e_C [] ;empty list
  let all_e_Ci (range initial (ends + interval) interval) ;new list with values of e_C for each behaviour
  foreach all_e_Ci[ ;edit list to avoid float problems, then define only one decimal.
    x -> let a precision x 1
    set all_e_C lput a all_e_C
  ]
  set each_group individuals / length all_e_C ;create turtle set based on settlement behaviours by calculate the total of behaviour and divide the number of individual for that.
  ask turtles [set e_C "NA"] ;then, assign e_C per group
  foreach all_e_C[
    x -> ask n-of each_group turtles with [e_C = "NA"] [set e_C x set color table:get list-colors x]
  ]
end

to turtle-colors
  set list-colors table:make
  table:put list-colors 0 15
  table:put list-colors 0.5 45
  table:put list-colors 1 25
  table:put list-colors 1.5 85
  table:put list-colors 2 135
end

;---------------------------------------------------------------------------------------------------
;----------------------------------------------OUTPUTS----------------------------------------------
;---------------------------------------------------------------------------------------------------

to cell_occupancy
  foreach all_e_C[
  x -> set occup_step count habitat with [count turtles-here with [status = "set" and e_C = x] > 0]
    table:put c_occupancy x occup_step
  ]
end

to save_movement_parameters
 if not file-exists? "output.txt"
  [file-open "movement_parameters.txt"
    file-print (word "run_number" "," "turtle" "," "t" "," "movement" "," "step_length" "," "turning_angle" "," "ang")
    file-close]
  file-open "movement_parameters.txt"
  file-print (word p "," who "," t "," movement "," l "," turning_angle "," ang)
  file-close
end

to turtle_path_set
  set turtle_path n-of N_path turtles
end

to save_paths
   if not file-exists? "paths.txt"
  [file-open "paths.txt"
    file-print (word "run_number" "," "who" "," "t" "," "xcor" "," "ycor" "," "cover of patch-here")
    file-close]
  ask turtle_path [
  file-open "paths.txt"
  file-print (word p "," who "," t "," new-xcor "," new-ycor "," [cover] of patch-here)
  file-close
  ]
end

to save_sim_parameters
  ;define run number
  ifelse not file-exists? "sim_parameters.txt"
   [file-open "sim_parameters.txt"
    file-print (word
        "run_number" "," "world-size" "," "patch_resolution" ","                       ;Environment
        "landscape_number" "," "hab_amount" "," "clumpiness" ","                       ;Landscape
        "specie" "," "perceptual_range" "," "initial_individuals" ","                  ;Agents
        "mortality_habitat" "," "mortality_matrix" "," "min_step_before_settle" ","    ;Movement
        "dmean" "," "dsd" ","                                                          ;Distance max per day
        "origin_site" "," "min_Ci" "," "var_Ci" "," "behaviours" ","                   ;Habitat requirements
        "discharge-rate" "," "En_mean" "," "En_sd" ","                                 ;Energetics
        "import_hab_quality_surface" "," "hab_quality_folder" ","                      ;Hab Quality Surface - import
        "hab_quality_per_frag" "," "ac_simple" "," "autocorrelation" "," "ac" ","      ;Hab Quality Surface - generators - by frag, by autocorrelation per frag, or total autocorrelation.
      )
    file-close
   ]
  [
    let file word output_directory "/sim_parameters.txt"
    r:put "file" file
    r:eval "x <- read.table(file = file ,sep = ',',header = T)"
    r:eval "y <- max(unique(x$run_number))"
    set p r:get "y"
    ifelse p < 0 [set p 0]
    [set p p + 1]
  ]
  ;storage parameters in temp file.
  file-open "sim_parameters.txt"
  file-print (word
        p "," world-size "," patch-resolution ","                                      ;Environment
        num_lands "," forest "," clumpiness ","                                        ;Landscape
        Specie "," perceptual_range "," individuals ","                                ;Agents
        mortality_habitat "," mortality_matrix "," min_step_before_settle ","          ;Movement
        dmean "," dsd ","                                                              ;Distance max per day
        origin_site "," min_Ci "," var_Ci "," all_e_C ","                              ;Habitat requirements
        discharge-rate "," En_mean "," En_sd ","                                       ;Energetics
        import_hab_quality_surface "," hab_quality_folder ","                          ;Hab Quality Surface - import
        hab_quality_per_frag "," ac_simple "," autocorrelation "," ac ","              ;Hab Quality Surface - generators - by frag, by autocorrelation per frag, or total autocorrelation.
      )
  file-close
end

to create_output
  if not file-exists? "output.txt"
    [file-open "output.txt"
       file-print (word "run_number"
        "," "turtle"  "," "initial_id_patch" "," "initial_patch" "," "min_En" "," "max_dist_per_day" "," "initial_Ci" "," "behaviour" ","                  ;initial information per turtle
        "xcor" "," "ycor" "," "status" "," "final_C" "," "final_En" "," "Hab_quality_area" "," "patch-q" "," "decision" "," "linear_dist" "," "total_dist" ;results per turtle
   )
      file-close]
end

to generate_output ;per individuo
  file-open "output.txt"
  file-print (word p
        "," who  "," initial_id "," initial_patch "," min_En "," max_dist "," Ci "," e_C ","                     ;initial information per turtle
        xcor "," ycor "," status "," C "," En "," H' "," patch-q "," decision "," linear_dist "," total_dist     ;results per turtle
   )
  file-close
end
@#$#@#$#@
GRAPHICS-WINDOW
485
202
1416
1134
-1
-1
1.8
1
10
1
1
1
0
1
1
1
-256
256
-256
256
1
1
1
ticks
30.0

INPUTBOX
35
896
450
972
landscape_directory
/home/kekuntu/Documents/phd_project/Chapter_2/Landscapes/100land_10res/landscape_100land_10res/
1
0
String

INPUTBOX
140
112
229
172
num_lands
0.0
1
0
Number

INPUTBOX
35
1052
449
1132
output_directory
/home/kekuntu/Documents/phd_project/Chapter_2/output
1
0
String

BUTTON
301
116
371
171
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
375
116
441
171
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
234
114
292
176
Landscape ID from database (0 to 99)
10
0.0
1

SLIDER
249
226
445
259
individuals
individuals
0
2000
500.0
5
1
NIL
HORIZONTAL

CHOOSER
35
120
127
165
Specie
Specie
"DA" "PQ" "MP"
0

SLIDER
249
267
445
300
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
1603
193
1763
313
Population
NIL
NIL
0.0
0.0
0.0
0.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if t > 1 [plot count turtles]"

SLIDER
34
188
231
221
mortality_habitat
mortality_habitat
0
0.05
0.002
0.001
1
NIL
HORIZONTAL

PLOT
1470
366
1721
496
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
35
981
450
1041
data_directory
/home/kekuntu/Documents/phd_project/Chapter_2/data
1
0
String

INPUTBOX
36
597
187
659
discharge-rate
0.01
1
0
Number

PLOT
1765
193
1952
313
Mean C of Dispersers
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
"0.0" 1.0 0 -2674135 true "" "if t > 1 and turtles with [status = \"dis\"] != 0 [plot mean [(C)] of turtles with [status = \"dis\" and e_C = 0.0]]"
"0.5" 1.0 0 -1184463 true "" "if t > 1 and turtles with [status = \"dis\"] != 0 [plot mean [(C)] of turtles with [status = \"dis\" and e_C = 0.5]]"
"1.0" 1.0 0 -955883 true "" "if t > 1 and turtles with [status = \"dis\"] != 0 [plot mean [(C)] of turtles with [status = \"dis\" and e_C = 1.0]]"
"1.5" 1.0 0 -11221820 true "" "if t > 1 and turtles with [status = \"dis\"] != 0 [plot mean [(C)] of turtles with [status = \"dis\" and e_C = 1.5]]"
"2.0" 1.0 0 -2064490 true "" "if t > 1 and turtles with [status = \"dis\"] != 0 [plot mean [(C)] of turtles with [status = \"dis\" and e_C = 2.0]]"

INPUTBOX
35
511
94
571
initial
0.0
1
0
Number

TEXTBOX
37
575
187
593
Energetic dynamics
12
0.0
1

TEXTBOX
34
487
232
517
Habitat Selection Behaviors
12
0.0
1

TEXTBOX
40
872
390
890
Directories (landscapes, input data, and output)\n
12
0.0
1

INPUTBOX
393
661
447
721
ac
0.0
1
0
Number

TEXTBOX
260
681
427
708
Complex: Autocorrelation\n of Habitat Quality (0-1)
10
0.0
1

INPUTBOX
97
511
151
571
interval
0.5
1
0
Number

INPUTBOX
154
511
210
571
ends
2.0
1
0
Number

PLOT
1725
498
1956
627
Settler by behaviour
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
"0.0" 1.0 0 -2674135 true "" "plot count turtles with [status = \"set\" and e_C = 0.0]"
"0.5" 1.0 0 -1184463 true "" "plot count turtles with [status = \"set\" and e_C = 0.5]"
"1.0" 1.0 0 -955883 true "" "plot count turtles with [status = \"set\" and e_C = 1.0]"
"1.5" 1.0 0 -11221820 true "" "plot count turtles with [status = \"set\" and e_C = 1.5]"
"2.0" 1.0 0 -2064490 true "" "plot count turtles with [status = \"set\" and e_C = 2.0]"

PLOT
1470
498
1721
628
Habitat Quality in Settlement
NIL
NIL
0.0
100.0
0.0
0.5
true
true
"" " ; set-plot-x-range 0 1\n ; set-histogram-num-bars 10\n ; set-plot-pen-mode 1"
PENS
"Hab" 1.0 0 -5298144 true "" "if (count turtles with [decision = \"H\"] != 0) [plot mean [H'] of turtles with [decision = \"H\"]]"
"Energy" 1.0 0 -14070903 true "" "if (count turtles with [decision = \"En\"] != 0) [plot mean [H'] of turtles with [decision = \"En\"]]"

SWITCH
38
806
290
839
movement_parameters_output
movement_parameters_output
0
1
-1000

SWITCH
37
768
226
801
occupancy_output
occupancy_output
0
1
-1000

SWITCH
231
768
385
801
path_outputs
path_outputs
0
1
-1000

PLOT
1481
56
1641
176
Histogram Q
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"  set-plot-x-range min [Q] of habitat max [Q] of habitat\n  set-histogram-num-bars 20\n  set-plot-pen-mode 1" "\n"
PENS
"default" 1.0 0 -16777216 true "" "histogram [Q] of habitat"

PLOT
1318
55
1478
176
Max Dist per Day
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"  set-plot-x-range min [max_dist] of turtles max [max_dist] of turtles\n  set-histogram-num-bars 20\n  set-plot-pen-mode 1" ""
PENS
"default" 1.0 0 -16777216 true "" "histogram [max_dist] of turtles"

INPUTBOX
35
287
108
347
dmean
1000.0
1
0
Number

INPUTBOX
109
287
177
347
dsd
100.0
1
0
Number

PLOT
1725
366
1956
496
Occupancy
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"0.0" 1.0 0 -2674135 true "" "if (t > 1) [plot table:get c_occupancy 0]"
"0.5" 1.0 0 -1184463 true "" "if (t > 1) [plot table:get c_occupancy 0.5]"
"1.0" 1.0 0 -955883 true "" "if (t > 1) [plot table:get c_occupancy 1]"
"1.5" 1.0 0 -11221820 true "" "if (t > 1) [plot table:get c_occupancy 1.5]"
"2.0" 1.0 0 -2064490 true "" "if (t > 1) [plot table:get c_occupancy 2]"

PLOT
1440
193
1600
313
Energetic Condition
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" "  set-plot-x-range 0 1\n  set-histogram-num-bars 20\n  set-plot-pen-mode 1"
PENS
"default" 1.0 0 -16777216 true "" "histogram [En] of turtles"

PLOT
1143
39
1303
188
Hab Quality C 2.0
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" "  set-plot-x-range 0 1\n ; set-plot-y-range 0 20\n  set-histogram-num-bars 20\n  set-plot-pen-mode 1\n  "
PENS
"2.0" 1.0 1 -2064490 true "" "histogram [H'] of turtles with [status = \"set\" and e_C = 2]"

PLOT
486
39
649
189
Hab Quality C 0.0
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" "  set-plot-x-range 0 1\n ; set-plot-y-range 0 20\n  set-histogram-num-bars 20\n  set-plot-pen-mode 1"
PENS
"default" 1.0 0 -2674135 true "" "histogram [H'] of turtles with [status = \"set\" and e_C = 0]"

PLOT
652
39
812
189
Hab Quality C 0.5
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" "  set-plot-x-range 0 1\n ; set-plot-y-range 0 20\n  set-histogram-num-bars 20\n  set-plot-pen-mode 1"
PENS
"default" 1.0 0 -4079321 true "" "histogram [H'] of turtles with [status = \"set\" and e_C = 0.5]"

PLOT
815
39
975
189
Hab Quality C 1.0
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" "  set-plot-x-range 0 1\n ; set-plot-y-range 0 20\n  set-histogram-num-bars 20\n  set-plot-pen-mode 1\n  "
PENS
"default" 1.0 0 -955883 true "" "histogram [H'] of turtles with [status = \"set\" and e_C = 1]"

PLOT
979
39
1139
189
Hab Quality C 1.5
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" "  set-plot-x-range 0 1\n ; set-plot-y-range 0 20\n  set-histogram-num-bars 20\n  set-plot-pen-mode 1\n  "
PENS
"default" 1.0 0 -11221820 true "" "histogram [H'] of turtles with [status = \"set\" and e_C = 1.5]"

TEXTBOX
35
268
240
298
Distance Maxima Per Day
12
0.0
1

INPUTBOX
389
768
457
828
N_path
5.0
1
0
Number

TEXTBOX
299
805
378
853
Save complete trajectories of N random turtles
10
0.0
1

TEXTBOX
40
741
249
771
Outputs
14
0.0
1

TEXTBOX
38
839
290
899
Save step lenght and turning angle of all individuals in Matrix and Habitat
10
0.0
1

BUTTON
1520
784
1884
819
show mean habitat quality in settlement per behaviour
let mean-Q table:make ;empty table\nforeach all_e_C[\n    x -> \n    let m mean [Q] of turtles with [e_C = x]\n    table:put mean-Q x m\n  ]\nshow mean-Q
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
1558
823
1855
856
show mean linear distance per behaviour
let mean-ld table:make ;empty table\nforeach all_e_C[\n    x -> \n    let m mean [linear_dist] of turtles with [e_C = x]\n    table:put mean-ld x m\n  ]\nshow mean-ld
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
1571
860
1843
893
show mean total distance per behaviour
let mean-td table:make ;empty table\nforeach all_e_C[\n    x -> \n    let m mean [total_dist] of turtles with [e_C = x]\n    table:put mean-td x m\n  ]\nshow mean-td
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
1549
915
1704
960
Decision by Quality
count turtles with [decision = \"H\"]
1
1
11

MONITOR
1710
915
1869
960
Decision by Energy
count turtles with [decision = \"En\"]
1
1
11

BUTTON
330
182
412
215
profiler
profiler:start         ;; start profiling\nsetup                  ;; set up the model\nrepeat 5 [ go ]       ;; run something you want to measure\nprofiler:stop          ;; stop profiling\nprint profiler:report  ;; view the results\nprofiler:reset         ;; clear the data\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
239
410
464
443
import_hab_quality_surface
import_hab_quality_surface
1
1
-1000

TEXTBOX
244
379
478
406
Choose to import a .asc file with habitat quality information, or to generate 
10
0.0
1

INPUTBOX
239
449
413
509
hab_quality_folder
hab_quality/256/
1
0
String

TEXTBOX
283
355
433
373
Habitat Quality Surface
12
0.0
1

SWITCH
243
546
448
579
hab_quality_per_frag
hab_quality_per_frag
1
1
-1000

TEXTBOX
239
514
489
556
Generating habitat quality surface, per fragment based on patches_ID layer
10
0.0
1

SLIDER
364
619
464
652
autocorrelation
autocorrelation
1
8
3.0
1
1
NIL
HORIZONTAL

SWITCH
246
619
361
652
ac_simple
ac_simple
0
1
-1000

TEXTBOX
244
589
463
622
Generating habitat quality surface based on spatial autocorrelation simple or complex
10
0.0
1

INPUTBOX
35
670
109
730
En_mean
0.2
1
0
Number

INPUTBOX
114
670
186
730
En_sd
0.1
1
0
Number

PLOT
1643
55
1803
175
Min Energy 
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"  set-plot-x-range min [min_En] of turtles max [min_En] of turtles\n  set-histogram-num-bars 20\n  set-plot-pen-mode 1" ""
PENS
"default" 1.0 0 -16777216 true "" "histogram [min_En] of turtles"

TEXTBOX
35
355
219
385
Initial Hab Quality Requirement (C)
12
0.0
1

SWITCH
35
387
152
420
origin_site
origin_site
1
1
-1000

TEXTBOX
1328
30
1548
64
Plots to check basic variables
14
0.0
1

TEXTBOX
492
10
804
31
Histogram of Settlers for each behaviour
14
0.0
1

SLIDER
248
306
447
339
min_step_before_settle
min_step_before_settle
0
10
5.0
1
1
NIL
HORIZONTAL

SLIDER
33
226
231
259
mortality_matrix
mortality_matrix
0
0.05
0.001
0.001
1
NIL
HORIZONTAL

PLOT
1806
54
1966
175
Ci
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"  set-plot-x-range min [Ci] of turtles max [Ci] of turtles\n  set-histogram-num-bars 20\n  set-plot-pen-mode 1" ""
PENS
"default" 1.0 0 -16777216 true "" "histogram [Ci] of turtles"

INPUTBOX
34
425
98
485
min_Ci
0.5
1
0
Number

INPUTBOX
101
425
166
485
var_Ci
0.2
1
0
Number

TEXTBOX
1478
338
1628
356
Basic Results
14
0.0
1

INPUTBOX
29
38
117
98
world-size
256.0
1
0
Number

INPUTBOX
122
38
208
98
patch-sizes
1.8
1
0
Number

INPUTBOX
213
38
331
98
patch-resolution
10.0
1
0
Number

TEXTBOX
29
11
179
29
Define World:
14
0.0
1

PLOT
1471
631
1720
764
Total Distance Moved
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"  ;set-plot-x-range 0 1\n  set-histogram-num-bars 20\n  set-plot-pen-mode 1" " if count turtles with [status = \"dis\"] = 0\n [ set-plot-x-range 0 max [total_dist] of turtles\n  set-plot-y-range 0 5\n  set-histogram-num-bars 20\n  set-plot-pen-mode 1\n  ]"
PENS
"pen-1" 1.0 0 -16777216 true "" "histogram [total_dist] of turtles"

PLOT
1725
631
1957
764
Linear Distance Moved
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" " if count turtles with [status = \"dis\"] = 0 [\n  set-plot-x-range 0 max [linear_dist] of turtles\n  set-plot-y-range 0 10\n  set-histogram-num-bars 20\n  set-plot-pen-mode 1\n  ]"
PENS
"default" 1.0 1 -16777216 true "" "histogram [linear_dist] of turtles"

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
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <final>r:stop</final>
    <enumeratedValueSet variable="world-size">
      <value value="256"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-sizes">
      <value value="1.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-resolution">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Specie">
      <value value="&quot;DA&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="num_lands" first="0" step="1" last="4"/>
    <enumeratedValueSet variable="mortality_matrix">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mortality_habitat">
      <value value="0.002"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_step_before_settle">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perceptual_range">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="individuals">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dmean">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dsd">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="origin_site">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="var_Ci">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_Ci">
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
    <enumeratedValueSet variable="En_mean">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="En_sd">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="discharge-rate">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data_directory">
      <value value="&quot;/home/kekuntu/Documents/phd_project/Chapter_2/data&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output_directory">
      <value value="&quot;/home/kekuntu/Documents/phd_project/Chapter_2/output&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="landscape_directory">
      <value value="&quot;/home/kekuntu/Documents/phd_project/Chapter_2/Landscapes/100land_10res/landscape_100land_10res/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="import_hab_quality_surface">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hab_quality_folder">
      <value value="&quot;hab_quality/256/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hab_quality_per_frag">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="autocorrelation">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ac_simple">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ac">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement_parameters_output">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="occupancy_output">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="path_outputs">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N_path">
      <value value="5"/>
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
