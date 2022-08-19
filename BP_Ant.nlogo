breed [antHills antHill]
breed [queens queen]
breed [males male]
breed [eggs egg]
breed [larvae larva]
breed [pupae pupa]
breed [workers worker]

breed [trees tree]
breed [bushes bush]
breed [berries berry]
breed [flowers flower]

breed [prey sPrey]
breed [antLions antLion]
breed [spiders spider]

globals [food1Total food2Total food3Total spiderKills lionKills energyKills germsKills antKills storedFoodTEST]

turtles-own [
  age
]

prey-own [
  hp
  webbed?
]

antlions-own [
  kills
]

spiders-own [
  hp
  victim
  fighting?
  kills
  counter
]

patches-own [
 pheromoneF
 pheromoneA
 pScent
 pMaterial
 pFood
 nest?
 tree?
 bush?
 flower?
 lionHole?
 web?
]

queens-own [
  energy
  scent
  homeNest
  germs
]

males-own [
  energy
  homeNest
]

eggs-own [
  scent
  homeNest
  germs
]

larvae-own [
  scent
  homeNest
  energy
  germs
]

pupae-own [
  scent
  homeNest
  germs
]

workers-own [
  energy
  food
  material
  scent
  victim
  homeNest
  fighting?
  carrying?
  young?
  webbed?
  foodSource? ; Variable helping to distinguish whether ant is carrying "prey" or material, OR the ant has found a food source. (To release the pheromone ant has to eat, in reality when they catch something they bring it to the anthill to get butchered.)
]

antHills-own [
  storedMaterial
  storedFood
  scent
  homeNest
  disrepair
]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      SETUP   PROCEDURES      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup
  clear-all
  initiate
  paint
  reset-ticks
end

to initiate
  ifelse swarms?
  [
    resize-world -50 50 -30 30
    clear
    spawnAntHill 0 -7 1 blue
    spawnAntHill -20 10 2 red
    spawnAntHill 20 10 3 yellow
    ifelse randomMap?
    [
      spawnTrees nTrees
      spawnBushes nBushes
    ]
    [
      makeTrees -48 -27
      makeTrees -46 14
      makeTrees -40 25
      makeTrees -42 -19
      makeTrees -36 19
      makeTrees -33 -25
      makeTrees -25 24
      makeTrees -20 -6
      makeTrees 48 25
      makeTrees 47 -24
      makeTrees 40 18
      makeTrees 42 -8
      makeTrees 35 24
      makeTrees 33 -10
      makeTrees 23 19
      makeTrees 20 -18
      makeTrees 18 1
      makeTrees 12 -27
      makeTrees -7 25
      makeTrees -2 -19
      makeTrees 8 -16
      makeTrees -10 4
      makeTrees 3 18
      makeBushes 0 7
      makeBushes 4 3
      makeBushes -3 5
      makeBushes -16 20
      makeBushes -14 -16
      makeBushes -32 5
      makeBushes 42 -4
      makeBushes 6 -24
      makeBushes -42 18
      makeBushes 36 10
      makeBushes 13 19
      makeBushes 25 -9
      makeBushes -39 -25
    ]
  ]
  [
    resize-world -30 30 -30 30
    clear
    spawnAntHill 0 0 1 blue
    ifelse randomMap?
    [
      spawnTrees nTrees
      spawnBushes nBushes
    ]
    [
      makeTrees -18 -17
      makeTrees -14 -20
      makeTrees -17 -8
      makeTrees -10 8
      makeTrees -19 22
      makeTrees -4 -6
      makeTrees 8 -17
      makeTrees 12 -4
      makeTrees 18 7
      makeTrees 14 19
      makeBushes 6 8
      makeBushes 11 -7
      makeBushes -8 15
      makeBushes -16 20
      makeBushes -14 -16
    ]
  ]



  ask anthills [
    spawnQueen
    spawnEggs (round (startingCount / 15)) (1)
    hatchLarvae (round (startingCount / 15)) (1)
    hatchPupae (round (startingCount / 5)) (1)
    let n round ((startingCount / 3))
    let m (startingCount - n)
    hatchWorkers n 0
    hatchWorkers m 10
  ]
  spawnFlowers nFlowers
  spawnPrey nPrey
  spawnLions nAntlions
  spawnSpiders nSpiders
end

to makeTrees [x y]
  create-trees 1 [
    set shape "tree"
    set color green - 3
    set size 4
    setxy x y
    ask patch-here [
      set tree? true
      ask neighbors [
        set tree? true
        ask neighbors4 [ set tree? true ]
      ]
    ]
  ]
end

to makeBushes [x y]
  create-bushes 1 [
    set shape "bush"
    set color green - 1
    set size 2
    setxy x y
    ask patch-here [
      set bush? true
      ask neighbors [
        if not tree? [ set bush? true ]
      ]
    ]
  ]
end

to paint
  ask patches [
    set pcolor green - 0.5
    if pMaterial > 0 [
      set pcolor brown
    ]
    if bush? [
      ifelse pFood > 0
        [ set pcolor blue - 3 ]
        [ set pcolor green - 2.5 ]
    ]
    if flower?
      [set pcolor yellow]
    if tree?
      [set pcolor brown - 2]
    if nest?
      [set pcolor black]
    if lionHole?
      [ set pcolor brown - 3 ]
    if web?
      [ set pcolor white ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       LOOP   PROCEDURE       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go
  actAnts
  movePrey
  checkWebbed
  catch
  actSpiders
  paint
  paintPheromones
  diffusePheromones
  death
  addFood spawnChance
  addPredators
  dropNeedle 50
  ageAll
  showStored
  tick
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   CREATE AGENTS PROCEDURES   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Spawns an anthill at coordinates X Y with scent s (int) and colour c.
to spawnAntHill [x y s c]
  create-antHills 1 [
    set shape "anthill"
    set size 5
    setxy x y
    set scent s
    set color c
    set homeNest self
    set storedFood initFood
    set disrepair 0
    set nest? true
    ask neighbors [
      set nest? true
    ]
  ]
end

; Spawns n eggs.
to spawnEggs [n m]
  hatch-eggs n [
    set shape "dot"
    set size 0.5
    ifelse (m = 1)
    [ set age random 5 ]
    [ set age 0 ]
    hide-turtle
  ]
end

; Spawns n larvae
to hatchLarvae [n m]
  hatch-larvae n [
    set shape "larva"
    set size 1
    ifelse (m = 1)
    [ set age random 3 ]
    [ set age 0 ]
    set energy 50
    hide-turtle
  ]
end

; Spawns n pupae.
to hatchPupae [n m]
  hatch-pupae n [
    set shape "dot"
    set size 0.5
    ifelse (m = 1)
    [ set age random 4 ]
    [ set age 0 ]
    hide-turtle
  ]
end

; Spawns n workers with age m.
to hatchWorkers [n m]
  hatch-workers n [
    set shape "ant"
    set size 2
    set age m
    set energy nEnergy
    set foodSource? false
    set fighting? false
    set carrying? false
    set webbed? false
    ifelse (age < 10)
    [ set young? true ]
    [ set young? false ]
    rt random 360
  ]
end

; Spawns a queen
to spawnQueen
  hatch-queens 1 [
    set shape "ant"
    set size 3
    set age 0
    set energy nEnergy
  ]
end

; Spawns up to n trees at patches where there isn't a nest and at least in-radius of 7 of each other. Upgrade: Make it work.
to spawnTrees [n]
  ask n-of n patches [
    ifelse any? trees in-radius 7 or any? antHills in-radius 6
    [ ]
    [ set tree? true
      ask neighbors [
        set tree? true
        ask neighbors4 [
          set tree? true
        ]
      ]
      sprout-trees 1 [
      set shape "tree"
      set size 2
      set color green - 3
      ]
    ]
  ]

;  create-trees n [
;    set shape "tree"
;    set color green - 3
;    set size 4
;    setxy random-pxcor random-pycor
;    while [tree?] of patches in-radius 4 [
;      setxy (random-pxcor) (round random-ycor)
;    ]
;    ask patch-here [
;      set tree? true
;      ask neighbors [
;        set tree? true
;        ask neighbors4 [ set tree? true ]
;      ]
;    ]
;  ]
end

; Spawns n bushes at patches where there are no trees nor nests.
to spawnBushes [n]
  create-bushes n [
    set shape "bush"
    set color green - 1
    set size 2
    setxy random-pxcor random-pycor
    while [[tree? or nest?] of patch-here = true] [
      setxy (round random-pxcor) (round random-pycor)
    ]
    ask patch-here [
      set bush? true
      ask neighbors [
        if not tree? [ set bush? true ]
      ]
    ]
  ]
end

; Spawns n berries on random bush.
to growBerries [n]
  if any? bushes [
    ask n-of n patches with [bush? = true] [
      sprout-berries 1 [
        set shape "dot"
        set color blue - 4
      ]
      set pcolor blue - 3
      set pFood 250
    ]
  ]
end

; Spawns n flowers at patches where there are no trees, bushes nor nests.
to spawnFlowers [n]
  create-flowers n [
    set shape "flower"
    set color yellow
    set size 2
    setxy random-pxcor random-pycor
    while [[tree? or bush? or nest?] of patch-here = true] [
      setxy (round random-pxcor) (round random-pycor)
    ]
    ask patch-here [
      set pFood 50
      set flower? true
    ]
  ]
end

; Spawns n of prey at random coordinates where isn't a tree.
to spawnPrey [n]
  create-prey n [
    set shape "bug"
    set color black
    set size 1
    setxy random-xcor random-ycor
    while [[tree?] of patch-here = true] [
      setxy random-xcor random-ycor
    ]
    set webbed? false
    set hp 25
  ]
end

; Spawns n antLions at patches where there are no trees, bushes nor nests.
to spawnLions [n]
  create-antLions n [
    set shape "antLion"
    set color red - 2
    set size 2
    setxy random-pxcor random-pycor
    while [[tree? or bush? or nest?] of patch-here = true] [
      setxy (round random-pxcor) (round random-pycor)
    ]
    ask patch-here [
      set lionHole? true
    ]
  ]
end

; Spawns n spiders at the x edges of the map.
to spawnSpiders [n]
ask n-of n patches with [ pxcor = min-pxcor or pxcor = max-pxcor ] [
    sprout-spiders 1 [
      set shape "spider"
      set color red - 2
      set size 3
      set hp 50
      set counter 0
      set fighting? false
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      DEATH   PROCEDURES      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Each tick draing 1 energy from all ants and adds 1 germ to queens and offspring. Extension: properly model remaining fauna.
; Kills ants if energy drop to zero. Kills queens and offspring if germs grow over 100.
; If antlions manage to catch 25 ants they "die".
; If all food from flowers or berries is extracted, or their age exceeds 8 they wither away.
to death
  let energyDrain 1

  ask workers [
    set energy (energy - energyDrain)
    if energy <= 0 [
      set energyKills (energyKills + 1)
      die
    ]
    if age = (24) [ set young? false ]
    if age > (120) [ die ]
  ]
  ask queens [
    set energy (energy - energyDrain)
    set germs (germs + 1)
    if energy <= 0 [
      set energyKills (energyKills + 1)
      die
    ]
    if germs > 100 [
      set germsKills (germsKills + 1)
      die
    ]
  ]
  ask eggs [
    set germs (germs + 1)
    if germs > 50 [
      set germsKills (germsKills + 1)
      die
    ]
  ]
  ask larvae [
    set energy (energy - energyDrain)
    set germs (germs + 1)
    if energy <= 0 [
      set energyKills (energyKills + 1)
      die
    ]
    if germs > 50 [
      set germsKills (germsKills + 1)
      die
    ]
    if age >= 6 [ die ]
  ]
  ask pupae [
    set germs (germs + 1)
    if germs > 50 [
      set germsKills (germsKills + 1)
      die
    ]
  ]
  ask antLions [  ;;; Let's pretend they create a pupa and hatch, ok?
    if kills >= 15 [
      ask patch-here [
        set lionHole? false
        ask neighbors4 [ set lionHole? false ]
      ]
      die
    ]
  ]
  ask flowers [
    if ([pfood] of patch-here  = 0) [
      ask patch-here [ set flower? false ]
      die
    ]
    if age > witherRate [
      ask patch-here [
        set pFood 0
        set flower? false
      ]
      die
    ]
  ]
  ask berries [
    if ([pfood] of patch-here  = 0) [ die ]
    if age > witherRate [
      ask patch-here [ set pfood 0 ]
      die
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      WORKER  PROCEDURES      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Hatches all of offspring if the conditions are met.
; If ants are younger than 10, they prefer to stay in the anthill tending to the brood, feeding the queen or maintaining the anthill. Older ants go outside to forage.
; Makes workers eat and queen to lay eggs.
; Extension: Implement anthill environment, aphid farms and breeding of new queens and males.
to actAnts
  hatchAll
  ask workers [
    set pScent (pScent + 5)
    ifelse [nest?] of patch-here and homeNest?
    [
      ifelse young?
      [
        ifelse random 2 = 0
        [
          tend
          feed
        ]
        [
          maintain
        ]
        if (hungry?) [ takeFood ]
      ]
      [ moveWorkers ]
    ]
    [ if not webbed? [ moveWorkers ] ]
    hideNested
  ]
  eat
  ask queens [ layEggs ]
end

; If workers are engaged in a fight, continue. Otherwise check if the ants is carrying anything, if so returns to the nest. Otherwise forages.
to moveWorkers
  ifelse fighting?
  [
    fight
    fd 0.1
  ]
  [
    ifelse carrying?
    [ return ]
    [ forage ]
    if patch-ahead 0.5 != nobody [
      while [ [tree?] of patch-ahead 0.5 ] [ wiggle ]
    ]
    forward 0.5
  ]
end

; If worker shares a patch with any source of food, eats some.
; If not wonders aimlessly avoiding trees until any prey, food or pheromone trail is encountered, if ant gets hungry not encountering any food picks up materail and goes back to the anthill.
to forage
  wiggle
  ifelse [pFood] of patch-here > 0
  [
    set foodSource? true
    set carrying? true
    set food (food + 5)
    ask patch-here [ set pFood (pFood - 5) ]
    increaseTotal scent 5
  ]
  [
    if ((pheromoneF >= 0.05) and (pheromoneF <= 5)) or (pheromoneA >= 1) [ followTrail ]
    if (any? (flowers) in-radius 2) [
      let foodSource one-of flowers in-radius 2
      face foodSource
    ]
    if (any? (berries) in-radius 2) [
      let foodSource one-of berries in-radius 2
      face foodSource
    ]
    ifelse energy < 150
    [
      if ([pMaterial] of patch-here > 0) [
        set carrying? true
        set material (material + 1)
        ask patch-here [ set pMaterial (pMaterial - 1) ]
      ]
    ]
    [ hunt ]
  ]
end

; If worker runs into any potentional prey, sets their target and engages.
; Always engages spiders and simple prey. Towards ants from other nests acts neutral unless there is a local superiority of at least +2 or there already is a fight in which case new arrivals join in.
to hunt
  ifelse any? spiders in-radius 1
  [
    set victim one-of spiders in-radius 1
    engage
  ]
  [
    ifelse any? workers with [scent != [scent] of myself] in-radius 1
    [
      if ((count workers with [scent = [scent] of myself] in-radius 3) > ((count workers with [scent != [scent] of self] in-radius 3) + 2)) or pheromoneA > 5
      [
        set victim one-of workers with [scent != [scent] of myself] in-radius 1
        engage
      ]
    ]
    [
      if any? prey in-radius 1
      [
        set victim one-of prey in-radius 1
        engage
      ]
    ]
  ]
end

; Releases the aggresion pheromone faces the prey and start fighting.
to engage
  set fighting? true
  set pheromoneA (pheromoneA + 1000)
  face victim
  fight
end

; Fights - wiggles a bit and makes the prey face the ant and releases agression pheromone.
; If fighting spider alongside at least 2 other workers decreases its hp by 1 each tick.
; If fighting another worker has 1/20 chance of killing it.
; If fighting simple prey decreases its hp by 1 each tick.
; The ant that manages to finish off the prey gets +10 food and sets for homenest.
to fight
  ifelse victim != nobody
  [
    set pheromoneA (pheromoneA + 5000)
    back 0.1
    wiggle
    ifelse ([breed] of victim = spiders)
    [
      if (count workers in-radius 3 with [fighting? = true] > 3) [
        ask victim [
          face myself
          set hp (hp - 1)
          if hp <= 0 [
            destroyWeb ; Spider can be finished off whilst in a net. If so destroy it.
            die
          ]
        ]
      ]
    ]
    [
      ifelse ([breed] of victim = workers)
      [
        ask victim [
          face myself
          set fighting? true
          set victim myself
          if random antWars < 1 [
            set antKills (antKills + 1)
            die
          ]
        ]
      ]
      [
        ask victim [
          face myself
          set hp (hp - 1)
          if hp <= 0 [ die ]
        ]
      ]
    ]
    if victim = nobody [
      set carrying? true
      set food 10
      set fighting? false
      increaseTotal scent 10
    ]
  ]
  [ set fighting? false ]
end

; Returns to home anthill to deliver food, material or when running low on energy to get food (to take some more than 50 be stored).
; Tracks the way to the nest with food pheromone, provided the ant got to eat from berries or flowers.
to return
  ifelse nest? and homeNest?
  [
    set foodSource? false
    set carrying? false
    let x food
    set food 0
    let y material
    set material 0
    ask antHills in-radius 3 [
      set storedFood (storedFood + x)
      set storedMaterial (storedMaterial + y)
    ]
    if (hungry?) [ takeFood ]
    rt 180
  ]
  [
    face homeNest
    if foodSource? [ set pheromoneF (pheromoneF + 10) ]
    wiggle
  ]
end

; If energy is running low gains +200 energy at the cost of 1 food. The ant must carry some food.
to eat
  ask workers [
    if energy < 200 [
      if food > 0 [
       set food (food - 1)
       set energy (energy + nutrition)
      ]
    ]
  ]
end

; Clears off germs from offspring and the queen.
to tend
  if (any? eggs with [ germs > 10 ] in-radius 3) [
    ask (one-of eggs with [ germs > 10 ] in-radius 3) [ set germs (germs - 5) ]
  ]
  if (any? larvae with [ germs > 10 ] in-radius 3) [
    ask (one-of larvae with [ germs > 10 ] in-radius 3) [ set germs (germs - 5) ]
  ]
  if (any? pupae with [ germs > 10 ] in-radius 3) [
    ask (one-of pupae with [ germs > 10 ] in-radius 3) [ set germs (germs - 5) ]
  ]
  if (any? queens with [ germs > 10 ] in-radius 3) [
    ask (one-of queens with [ germs > 10 ] in-radius 3) [ set germs (germs - 5) ]
  ]
end

; If larvae with energy less than 1000 are present and more than 30 food is stored, feed larvae.
; If a queen with energy less than 1000 is present and there is any food stored, feed her. If the food is running low go forage.
to feed
  ifelse (any? queens with [ energy < 500 ] in-radius 3)
  [
    if storedFood? > 0
    [
      ask antHills in-radius 3
      [ set storedFood (storedFood - 1) ]
      ask (one-of queens with [ energy < 500 ] in-radius 3) [
        set energy (energy + nutrition)
      ]
    ]
  ]
  [
    if (any? larvae with [ energy < 5 * nEnergy ] in-radius 3) [
      if storedFood? > 10 [
        ask antHills in-radius 3
        [ set storedFood (storedFood - 1) ]
        ask (one-of larvae with [ energy < 5 * nEnergy ] in-radius 3) [ set energy (energy + nutrition) ]
      ]
    ]
  ]
  if (storedFood? < 10) [ moveWorkers ]
end

; If more than 50 food is stockpiled, takes 2 food.
to takeFood
  if ( storedFood? > 50 ) [
    set food (food + 2)
    ask antHills in-radius 3 [ set storedFood (storedFood - 2) ]
  ]
end

to-report hungry?
  ifelse food = 0
  [ report true ]
  [ report false ]
end

to maintain
  if (any? antHills with [ disrepair > 10 ] in-radius 3) [
    if storedMaterial? > 0 [
      ask antHills in-radius 3 [
        set storedMaterial (storedMaterial - 1)
        set disrepair (disrepair - 10)
      ]
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   REPRODUCTION  PROCEDURES   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Every week queens lays random of 8 to 15  eggs, provided they have enough energy.
to layEggs
  if ticks mod week = 0 [
    if energy > 500 [
      set energy (energy - 300)
      spawnEggs (random 8 + 8) 0
    ]
  ]
end

; At age of 5*modelSpeed, becomes a larva.
to hatchEgg
  if age >= 5  [
    hatchLarvae 1 0
    die
  ]
end

; At age of 3*hatchingSpeed if have been well fed, becomes a pupa.
to hatchLarva
  if age >= 3 [
    if energy > 5 * nEnergy [
      hatchPupae 1 0
      die
    ]
  ]
end

; At age of 4*hatchingSpeed, becomes new worker.
to hatchPupa
  if age >= 4  [
    hatchWorkers 1 0
    die
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    PHEROMONE   PROCEDURES    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; If there are pheromones up ahead sets in their direction.
to followTrail
  let frontP 0
  let rightP 0
  let leftP 0

  let FP concentrationAt 0 2
  let AP concentrationAt 0 1
  ifelse FP > AP
  [ set frontP FP ]
  [ set frontP AP ]
  set FP concentrationAt 45 2
  set AP concentrationAt 45 1
  ifelse FP > AP
  [ set rightP FP ]
  [ set rightP AP ]
  set FP concentrationAt -45 2
  set AP concentrationAt -45 1
  ifelse FP > AP
  [ set leftP FP ]
  [ set leftP AP ]
  if (rightP > frontP) or (leftP > frontP)
  [
    ifelse rightP > leftP
    [ rt 45 ]
    [ lt 45 ]
  ]
end

; Returns the strength of pheromones on the patches straight ahead and at +-angles. 0 for scent, 1 for food pheromone, 2 for attack pheromone.
to-report concentrationAt [angle pheromone]
  let p patch-right-and-ahead angle 1
  if p = nobody [ report 0 ]
  if pheromone = 0 [ report [ pScent ] of p ]
  if pheromone = 1 [ report [ pheromoneA ] of p ]
  if pheromone = 2 [ report [ pheromoneF ] of p ]
end

; Makes strength of pheromones visible on the map.
to paintPheromones ;
  ask patches [
    if showPS
      [if pScent > 1 [set pcolor scale-color blue pScent -10 30] ]
    if showPF
      [if pheromoneF > 1 [ set pcolor scale-color green pheromoneF -10 30] ]
    if showPA
      [if pheromoneA > 5 [set pcolor scale-color red pheromoneA -10 30] ]
  ]
end

; Makes the strength of pheromones and scent spread to neighboring patches.
to diffusePheromones
  let diffusionRateS 0.05
  diffuse pScent diffusionRateS
  let diffusionRateF 0.05
  diffuse pheromoneF diffusionRateF
  let diffusionRateA 0.5
  diffuse pheromoneA diffusionRateA
  ask patches [
    set pScent pScent * (1 - diffusionRateS)
    if pScent < 0.005 [
    set pScent 0
    ]
    set pheromoneF pheromoneF * (1 - diffusionRateF)
    if pheromoneF < 0.005 [
      set pheromoneF 0
    ]
    set pheromoneA pheromoneA * (1 - diffusionRateA)
    if pheromoneA < 1 [
      set pheromoneA 0
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    OTHER FAUNA PROCEDURES    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Wiggles and moves prey. Avoids tree patches.
to movePrey
  ask prey [
    if not webbed? [
      wiggle
      if patch-ahead 0.5 != nobody [
        while [ [tree?] of patch-ahead 0.5 ] [ wiggle ]
      ]
      forward 0.3
    ]
  ]
end

; Creates web at neighbor4.
to weaveWeb
  ask patch-here [
    set web? true
    ask neighbors [ set web? true ]
  ]
end

; Every 150 ticks rellocates or 10 ant kills. If any yet uneaten workers or prey remain in the web, release them.
; Imitates spiders searching for a new place after the web gets damaged or at they need a better spot as nothing gets caught.
to relocate
  if (counter = spiderStability) or (kills >= 10) [
    set counter 0
    destroyWeb
    set kills 0
    wiggle
    forward 2
  ]
end

to destroyWeb
  ask patch-here [
    set web? false
    ask neighbors [ set web? false ]
    if any? workers in-radius 2 [
      ask workers in-radius 2 [ set webbed? false ]
    ]
    if any? prey in-radius 2 [
      ask prey in-radius 2 [ set webbed? false ]
    ]
  ]
end

; Searches for a new tree/bush to settle. If the spider encounters any ants engages them.
to actSpiders
  ask spiders [
    ifelse fighting?
    [ fightAnts ]
    [
      ifelse (any? bushes in-cone-nowrap 2 330 or any? trees in-cone-nowrap 2 330)
      [
        ifelse ( web? = true )
        [
          set counter (counter + 1)
          relocate
        ]
        [ weaveWeb ]
      ]
      [
        wiggle
        if (not any? anthills in-cone-nowrap 4 330) and (any? workers in-cone-nowrap 3 330) [
          set victim one-of workers in-cone-nowrap 3 330
          face victim
          set fighting? true
        ]
        forward 0.5
      ]
    ]
  ]
end

; If spider encounters any ants engages them. With probability of 1 in 10 kills said ant.
to fightAnts
  ifelse victim != nobody
  [
    back 0.1
    wiggle
    ask victim [ face myself ]
    if random 10 < 1 [
      ask victim [ die ]
      set spiderKills (spiderKills + 1)
    ]
    if victim != nobody [
      if any? workers in-cone-nowrap 2 330 [
        set victim one-of workers in-cone-nowrap 2 330
        face victim
      ]
    ]
    forward 0.1
  ]
  [ set fighting? false ]
end

; If there is any prey or workers caught in the web, or adjacent to the antlion, eat them and adds to kill count.
to catch
  ask spiders [
    ifelse any? workers with [webbed? = true] in-radius 2
    [
      ask one-of workers in-radius 2 [ die ]
      set kills (kills + 1)
      set spiderKills (spiderKills + 1)
    ]
    [
      if any? prey with [webbed? = true] in-radius 2 [
        ask one-of prey in-radius 2 [ die ]
      ]
    ]
  ]
  ask antLions [
    if any? workers in-radius 1 [
      ask one-of workers in-radius 1 [ die ]
      set kills (kills + 1)
      set lionKills (lionKills + 1)
    ]
  ]
end

; If the amount of spiders drops to 0 spawns up to 3 new ones, if the antlions count drop to 1 spawns up to 3 new ones.
to addPredators
  if count spiders < (nSpiders / 2) [ spawnSpiders (nSpiders - count spiders)  ]
  if count antlions < ( nAntlions / 2) [ spawnLions (nAntlions - count antlions) ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      FLORA   PROCEDURES      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; With probability of 1/n drops needles in radius 10 around trees.
to dropNeedle [n]
  if random n < 1 [
    ask trees [
      ask one-of patches in-radius 10 with [tree? = false] [
        set pMaterial (pMaterial + 1)
      ]
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    AUXILLARY   PROCEDURES    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Returns true/false. Namely whether the nest shares the scent of the ant.
to-report homeNest?
  let hN? false
  if any? antHills with [scent = [scent] of self] in-radius 3
  [ set hN? true ]
  report hN?
end

; Returns int. Namely the amount of stored material.
to-report storedMaterial?
  let sM? 0
  ask antHills in-radius 3 [
    set sM? storedMaterial
  ]
  report sM?
end

; Returns int. Namely the amount of stored food.
to-report storedFood?
  let sF? 0
  ask antHills in-radius 3 [
    set sF? storedFood
  ]
  report sF?
end

; With probability of 1/n grows up to 3 berries, up to 5 flowers and spawns up to 2 prey.
to addFood [n]
  if random n < 1 [
    let m round (random spawnRate)
    growBerries m
  ]
  if random n < 1 [
    let m round (random 2 * spawnRate)
    spawnFlowers m
  ]
  if random n < 1 [
    let m round (random spawnRate)
    spawnPrey m
  ]
end

; Wiggles random angle [0;45] rightwards and then leftwards. If at the edge of the map turn 180°.
to wiggle
  ifelse random 2 = 1
  [ rt random 45 ]
  [ lt random 45 ]
  if not can-move? 1
  [ rt 180 ]
end

; If any workers or prey stepped in web, set webbed? true.
to checkWebbed
  ask workers [
    if [web?] of patch-here [ set webbed? true ]
  ]
  ask prey [
    if [web?] of patch-here [ set webbed? true ]
  ]
end

; Hatches eggs, larvae and pupae.
to hatchAll
  ask eggs [ hatchEgg ]
  ask larvae [ hatchLarva ]
  ask pupae [ hatchPupa ]
end

; Hides nested ants, doesn't hide the queen.
to hideNested
  ifelse ([nest?] of patch-here = true)
  [ hide-turtle ]
  [ show-turtle ]
end

;Every 33 ticks increases age by 1.
to ageAll
  if ticks mod week = 0 [
    ask turtles [
      set age (age + 1)
    ]
  ]
end

to increaseTotal [s f]
  if (s = 1) [ set food1Total (food1Total + f) ]
  if (s = 2) [ set food2Total (food2Total + f) ]
  if (s = 3) [ set food3Total (food3Total + f) ]
end

to clear
  ask patches [
    set nest? false
    set tree? false
    set bush? false
    set flower? false
    set lionHole? false
    set web? false
    set pFood 0
  ]
end

; UI procedure. Shows stored food and materials in all anthills.
to showStored
  ask antHills [
    set storedFoodTEST storedFood
    ask patch-at 3 -3 [
      set plabel (word "Stored Food: " " " precision [storedFood] of myself 0)
    ]
    ask patch-at 3 -4 [
      set plabel (word "Stored Material: " " " precision [storedMaterial] of myself 0)
    ]
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
210
10
1228
629
-1
-1
10.0
1
10
1
1
1
0
0
0
1
-50
50
-30
30
1
1
1
ticks
30.0

BUTTON
13
333
101
366
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
106
333
194
366
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

SLIDER
13
10
195
43
nTrees
nTrees
0
30
25.0
1
1
NIL
HORIZONTAL

SLIDER
12
46
194
79
nBushes
nBushes
0
30
17.0
1
1
NIL
HORIZONTAL

SLIDER
13
82
195
115
nFlowers
nFlowers
0
100
50.0
1
1
NIL
HORIZONTAL

SLIDER
13
118
195
151
nPrey
nPrey
0
100
25.0
1
1
NIL
HORIZONTAL

SWITCH
12
369
102
402
showPF
showPF
0
1
-1000

SWITCH
104
369
194
402
showPA
showPA
0
1
-1000

SLIDER
10
473
192
506
spawnChance
spawnChance
5
30
10.0
1
1
1/n
HORIZONTAL

SLIDER
13
154
195
187
startingCount
startingCount
30
50
40.0
2
1
NIL
HORIZONTAL

SWITCH
12
403
102
436
showPS
showPS
0
1
-1000

PLOT
211
632
711
815
Collected Food (Total)
Time
Food Collected
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"Brood 1" 1.0 0 -13345367 true "" "plot food1Total "
"Brood 2" 1.0 0 -2674135 true "" "plot food2Total"
"Brood 3" 1.0 0 -1184463 true "" "plot food3Total"

PLOT
727
632
1228
815
Food Collection Rate
Time
Collection Rate
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"Brood 1" 1.0 0 -13345367 true "" "plot sum [food] of workers with [scent = 1]"
"Brood 2" 1.0 0 -2674135 true "" "plot sum [food] of workers with [scent = 2]"
"Brood 3" 1.0 0 -1184463 true "" "plot sum [food] of workers with [scent = 3]"

PLOT
1235
327
1784
629
Size of Brood p2
Time
Offspring Count
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"Brood 1 Eggs" 1.0 0 -14730904 true "" "plot count eggs with [scent = 1]"
"Brood 1 Larvae" 1.0 0 -13345367 true "" "plot count larvae with [scent = 1]"
"Brood 1 Pupae" 1.0 0 -8020277 true "" "plot count pupae with [scent = 1]"
"Brood 2 Eggs" 1.0 0 -8053223 true "" "plot count eggs with [scent = 2]"
"Brood 2 Larvae" 1.0 0 -2674135 true "" "plot count larvae with [scent = 2]"
"Brood 2 Pupae" 1.0 0 -1604481 true "" "plot count pupae with [scent = 2]"
"Brood 3 Eggs" 1.0 0 -7171555 true "" "plot count eggs with [scent = 3]"
"Brood 3 Larvae" 1.0 0 -1184463 true "" "plot count larvae with [scent = 3]"
"Brood 3 Pupae" 1.0 0 -723837 true "" "plot count pupae with [scent = 3]"

PLOT
1235
10
1784
312
Size of Brood
Time
Worker Count
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"Brood 1 " 1.0 0 -13345367 true "" "plot count workers with [scent = 1]"
"Brood 2" 1.0 0 -2674135 true "" "plot count workers with [scent = 2]"
"Brood 3" 1.0 0 -1184463 true "" "plot count workers with [scent = 3]"

MONITOR
1236
634
1342
679
Killed by spiders
spiderKills
17
1
11

MONITOR
1236
680
1346
725
Killed by antlions
lionKills
17
1
11

MONITOR
1236
726
1367
771
Death of energy loss
energyKills
17
1
11

MONITOR
1370
636
1494
681
Killed by other hives
antKills
17
1
11

SLIDER
13
190
195
223
nSpiders
nSpiders
0
10
3.0
1
1
NIL
HORIZONTAL

SLIDER
13
225
196
258
nAntlions
nAntlions
0
15
5.0
1
1
NIL
HORIZONTAL

SLIDER
13
260
195
293
initFood
initFood
100
1000
500.0
50
1
NIL
HORIZONTAL

SLIDER
11
614
192
647
nEnergy
nEnergy
150
500
280.0
10
1
NIL
HORIZONTAL

SLIDER
11
650
193
683
antWars
antWars
10
100
30.0
5
1
NIL
HORIZONTAL

SLIDER
11
578
193
611
nutrition
nutrition
100
400
230.0
10
1
NIL
HORIZONTAL

SLIDER
11
685
192
718
spiderStability
spiderStability
50
300
148.0
1
1
NIL
HORIZONTAL

SLIDER
11
438
192
471
week
week
60
200
85.0
2
1
NIL
HORIZONTAL

SLIDER
11
509
192
542
spawnRate
spawnRate
1
10
2.0
1
1
NIL
HORIZONTAL

SWITCH
13
296
194
329
randomMap?
randomMap?
1
1
-1000

SLIDER
10
543
193
576
witherRate
witherRate
2
10
5.0
1
1
NIL
HORIZONTAL

SWITCH
105
404
195
437
swarms?
swarms?
0
1
-1000

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

ant
true
0
Circle -16777216 true false 105 165 90
Circle -16777216 true false 113 31 74
Circle -16777216 true false 128 98 44
Circle -16777216 true false 120 150 60
Circle -16777216 true false 120 210 60
Polygon -16777216 true false 135 21 135 51 165 51 165 21 150 36 135 21 135 21
Line -16777216 false 105 15 135 60
Line -16777216 false 195 15 165 60
Line -16777216 false 90 75 135 105
Line -16777216 false 165 105 210 75
Line -16777216 false 90 75 75 90
Line -16777216 false 210 75 225 90
Line -16777216 false 75 105 135 120
Line -16777216 false 75 105 60 120
Line -16777216 false 165 120 225 105
Line -16777216 false 225 105 240 120
Line -16777216 false 135 150 75 135
Line -16777216 false 75 135 60 150
Line -16777216 false 165 150 225 135
Line -16777216 false 225 135 240 150
Circle -16777216 true false 128 128 44
Circle -7500403 true true 135 135 30
Circle -7500403 true true 135 105 30
Circle -7500403 true true 128 158 44
Circle -7500403 true true 119 179 62
Circle -7500403 true true 128 218 44
Circle -7500403 true true 128 53 44

anthill
false
0
Circle -6459832 true false 96 171 108
Circle -6459832 true false 51 171 108
Circle -6459832 true false 141 171 108
Circle -6459832 true false 66 126 108
Circle -6459832 true false 126 126 108
Circle -6459832 true false 96 81 108

antlion
false
0
Polygon -6459832 true false 120 180 75 195 45 225 255 225 225 195 180 180
Circle -7500403 true true 90 120 90
Circle -7500403 true true 120 120 90
Polygon -7500403 true true 120 165 105 135 105 120 105 105 120 75 150 45 135 90 135 165
Polygon -7500403 true true 180 165 195 135 195 120 195 105 180 75 150 45 165 90 165 165

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

bush
false
0
Circle -7500403 true true 55 45 130
Circle -7500403 true true 20 81 108
Circle -7500403 true true 161 86 127
Circle -7500403 true true 29 89 152
Circle -7500403 true true 119 89 152
Circle -7500403 true true 116 56 127

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

larva
false
0
Circle -7500403 true true 105 105 90
Circle -7500403 true true 63 123 84
Circle -7500403 true true 153 123 84
Circle -7500403 true true 30 150 60
Circle -7500403 true true 210 150 60

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

spider
true
0
Circle -7500403 true true 90 150 120
Circle -7500403 true true 113 31 74
Circle -7500403 true true 128 98 44
Circle -7500403 true true 105 135 90
Circle -7500403 true true 105 195 90
Polygon -7500403 true true 135 21 135 51 165 51 165 21 150 36 135 21 135 21
Line -7500403 true 90 75 135 105
Line -7500403 true 165 105 210 75
Line -7500403 true 90 75 75 90
Line -7500403 true 210 75 225 90
Line -7500403 true 75 105 135 120
Line -7500403 true 75 105 60 120
Line -7500403 true 165 120 225 105
Line -7500403 true 225 105 240 120
Line -7500403 true 135 120 75 135
Line -7500403 true 75 135 60 150
Line -7500403 true 165 120 225 135
Line -7500403 true 225 135 240 150
Circle -7500403 true true 128 128 44
Line -7500403 true 135 135 90 150
Line -7500403 true 165 135 210 150
Line -7500403 true 210 150 225 165
Line -7500403 true 90 150 75 165
Circle -2674135 true false 138 35 10
Circle -2674135 true false 152 35 10
Circle -2674135 true false 129 38 7
Circle -2674135 true false 164 38 7
Circle -2674135 true false 125 45 7
Circle -2674135 true false 134 44 7
Circle -2674135 true false 168 45 7
Circle -2674135 true false 158 45 7

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
  <experiment name="Baseline 1" repetitions="3" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <exitCondition>(count queens = 0)
or 
(storedFoodTEST &gt; 2000)</exitCondition>
    <metric>count workers</metric>
    <metric>food1Total</metric>
    <metric>storedFoodTEST</metric>
    <metric>count [bushes in-radius 10] of patch 0 0</metric>
    <metric>count [bushes in-radius 20] of patch 0 0</metric>
    <metric>count [bushes in-radius 30] of patch 0 0</metric>
    <enumeratedValueSet variable="nTrees">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spawnChance">
      <value value="7"/>
      <value value="14"/>
      <value value="21"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spiderStability">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nutrition">
      <value value="120"/>
      <value value="240"/>
      <value value="360"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="showPS">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nPrey">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="showPF">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nAntlions">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nSpiders">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initFood">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="startingCount">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="showPA">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nEnergy">
      <value value="150"/>
      <value value="300"/>
      <value value="450"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="week">
      <value value="60"/>
      <value value="120"/>
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="antWars">
      <value value="35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nFlowers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nBushes">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spawnRate">
      <value value="1"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="witherRate">
      <value value="2"/>
      <value value="6"/>
      <value value="10"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Baseline 2" repetitions="3" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <exitCondition>(count queens = 0)
or 
(storedFoodTEST &gt; 2000)</exitCondition>
    <metric>count workers</metric>
    <metric>food1Total</metric>
    <metric>storedFoodTEST</metric>
    <metric>count [bushes in-radius 10] of patch 0 0</metric>
    <metric>count [bushes in-radius 20] of patch 0 0</metric>
    <metric>count [bushes in-radius 30] of patch 0 0</metric>
    <enumeratedValueSet variable="nTrees">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spawnChance">
      <value value="1"/>
      <value value="15"/>
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spiderStability">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nutrition">
      <value value="100"/>
      <value value="220"/>
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="showPS">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nPrey">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="showPF">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nAntlions">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nSpiders">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initFood">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="startingCount">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="showPA">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nEnergy">
      <value value="150"/>
      <value value="270"/>
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="week">
      <value value="60"/>
      <value value="80"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="antWars">
      <value value="35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nFlowers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nBushes">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spawnRate">
      <value value="1"/>
      <value value="2"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="witherRate">
      <value value="2"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Baseline 3 - Plus Threats" repetitions="2" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <exitCondition>(count queens = 0)
or 
(storedFoodTEST &gt; 2000)</exitCondition>
    <metric>count workers</metric>
    <metric>food1Total</metric>
    <metric>storedFoodTEST</metric>
    <metric>count [bushes in-radius 10] of patch 0 0</metric>
    <metric>count [bushes in-radius 20] of patch 0 0</metric>
    <metric>count [bushes in-radius 30] of patch 0 0</metric>
    <enumeratedValueSet variable="nTrees">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spawnChance">
      <value value="15"/>
      <value value="22"/>
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spiderStability">
      <value value="50"/>
      <value value="150"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nutrition">
      <value value="230"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="showPS">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nPrey">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="showPF">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nAntlions">
      <value value="4"/>
      <value value="8"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nSpiders">
      <value value="4"/>
      <value value="8"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initFood">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="startingCount">
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="showPA">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nEnergy">
      <value value="280"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="week">
      <value value="85"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="antWars">
      <value value="35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nFlowers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nBushes">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spawnRate">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="witherRate">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="3 Swarms - 1" repetitions="2" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <exitCondition>(count queens = 0)
or
(count workers with [scent = 1] &lt; 10)
or
(count workers with [scent = 2] &lt; 10)
or
(count workers with [scent = 3] &lt; 10)</exitCondition>
    <metric>count workers with [scent = 1]</metric>
    <metric>count workers with [scent = 2]</metric>
    <metric>count workers with [scent = 3]</metric>
    <metric>food1Total</metric>
    <metric>food2Total</metric>
    <metric>food3Total</metric>
    <enumeratedValueSet variable="nTrees">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spawnChance">
      <value value="15"/>
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="randomMap?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spawnRate">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spiderStability">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nutrition">
      <value value="220"/>
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="showPS">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nPrey">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="showPF">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nAntlions">
      <value value="6"/>
      <value value="12"/>
      <value value="18"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nSpiders">
      <value value="6"/>
      <value value="12"/>
      <value value="18"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="witherRate">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="startingCount">
      <value value="35"/>
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initFood">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="showPA">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="antWars">
      <value value="12"/>
      <value value="24"/>
      <value value="36"/>
      <value value="48"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nEnergy">
      <value value="270"/>
      <value value="280"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="week">
      <value value="80"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nFlowers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nBushes">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Experiment1" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <exitCondition>(count queens = 0)</exitCondition>
    <metric>count workers</metric>
    <metric>food1Total</metric>
    <metric>storedFoodTEST</metric>
    <metric>spiderKills</metric>
    <metric>lionKills</metric>
    <metric>energyKills</metric>
    <enumeratedValueSet variable="spawnChance">
      <value value="22"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spawnRate">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="witherRate">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nutrition">
      <value value="230"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nEnergy">
      <value value="280"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="week">
      <value value="85"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initFood">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="startingCount">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nAntlions">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nSpiders">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spiderStability">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="antWars">
      <value value="35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nTrees">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nFlowers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nBushes">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nPrey">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="showPS">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="showPF">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="showPA">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Experiment2" repetitions="2" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <exitCondition>(count queens = 0)</exitCondition>
    <metric>count workers</metric>
    <metric>food1Total</metric>
    <metric>storedFoodTEST</metric>
    <metric>spiderKills</metric>
    <metric>lionKills</metric>
    <metric>energyKills</metric>
    <enumeratedValueSet variable="spawnChance">
      <value value="22"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spawnRate">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="witherRate">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nutrition">
      <value value="230"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nEnergy">
      <value value="280"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="week">
      <value value="85"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initFood">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="startingCount">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nAntlions">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nSpiders">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spiderStability">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="antWars">
      <value value="35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nTrees">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nFlowers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nBushes">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nPrey">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="showPS">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="showPF">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="showPA">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="3 Swarms Baseline 1" repetitions="2" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <exitCondition>(count queens = 0)
or 
(storedFoodTEST &gt; 2000)</exitCondition>
    <metric>count workers with [scent = 1]</metric>
    <metric>count workers with [scent = 2]</metric>
    <metric>count workers with [scent = 3]</metric>
    <metric>food1Total</metric>
    <metric>food2Total</metric>
    <metric>food3Total</metric>
    <enumeratedValueSet variable="spawnChance">
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
      <value value="20"/>
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spawnRate">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="witherRate">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nutrition">
      <value value="230"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nEnergy">
      <value value="280"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="week">
      <value value="85"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initFood">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="startingCount">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nAntlions">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nSpiders">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spiderStability">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="antWars">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nTrees">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nFlowers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nBushes">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nPrey">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="showPS">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="showPF">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="showPA">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="3 Swarms Baseline 2 - Plus Threats" repetitions="2" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <exitCondition>(count queens = 0)
or 
(storedFoodTEST &gt; 2000)</exitCondition>
    <metric>count workers with [scent = 1]</metric>
    <metric>count workers with [scent = 2]</metric>
    <metric>count workers with [scent = 3]</metric>
    <metric>food1Total</metric>
    <metric>food2Total</metric>
    <metric>food3Total</metric>
    <enumeratedValueSet variable="spawnChance">
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spawnRate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="witherRate">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nutrition">
      <value value="230"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nEnergy">
      <value value="280"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="week">
      <value value="85"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initFood">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="startingCount">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nAntlions">
      <value value="2"/>
      <value value="6"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nSpiders">
      <value value="2"/>
      <value value="6"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spiderStability">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="antWars">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nTrees">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nFlowers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nBushes">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nPrey">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="showPS">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="showPF">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="showPA">
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
