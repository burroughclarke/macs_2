
;; These variables store statistics on what happens in the environment:
globals [
  random-variation
  starve-deaths    ;; how many organisms are killed by starvation?
  murder-deaths    ;; how many organisms are killed by other organisms?
]

;; turtles do not have breed-scope attributes: this is not an agent-based approach.
;; (in other words, there is no variable that represents what every creature in a breed does)
;; Evolution has no species-level scope, it is only concerned with each individual organism
;; organisms varies *individually* (or as pairs in sexual reproduction)
;; Each species displays variation caused by the average variation of individuals.

;; A single id value, 'minibreed', is stored in order, for example, for creatures of the same
;; breed to know not eat each other. It conferes no 'differentiating' effect on a breed-scope level.
;; Attributes that actually do affect behaviour, such as speed, size, etc, are all the same at initialisation.

;; attributes of 'organisms'
turtles-own
[
  minibreed           ;; this identifying number is the only seperating characteristic of each breed
  energy              ;; energy stored in organism. needed for breeding, moving, etc. If it reaches zero, it dies
  speed               ;; distance an organism can move each time interval
  flockmates          ;; agentset of all nearby creatures, used to select other creatures to eat
  nearest-neighbor    ;; details about the closest
  breed-rate          ;;
  herbivore-carnivore ;; optimisation towards getting energy from grass or energy from creatures
]

;; attributes of 'grass' patches
patches-own [
  countdown
]

to setup
  clear-all

  ;; grass patches are fully grown at startup

  ask patches [ set pcolor green ]

  ;; 'breednum' is a variable that is used in a loop to initialise each breed
  ;; to a different consecutive value (e.g. 3, 2, 1, 0)

  let breednum initial-number-of-breeds

  ;; there is always just one initial organism abstraction for each breed.

  create-turtles breednum  ; create the turtles, then initialize their variables
  [
    ;; assign a random shape and colour value to each breed, so each breed can be easily viewed on the map
    set color one-of [ red blue yellow pink orange black white grey cyan lime turquoise sky violet magenta] ;; don't want green!
    set shape one-of [ "bug" "butterfly" "cow" "bird" "fish" "person" "turtle" "sheep" "wolf" "ant" "cow skull" "dog" "ghost" "hawk" "rabbit" "shark" "spider" "wolf 2" "wolf 3" "wolf 4" "wolf 5" "wolf 6" "wolf 7" ]

    set minibreed breednum

    ;; set the initial variables, as defined by the user the GUI.
    ;; If the wrong initial values are set, the creatures cannot
    ;; 'evolve' fast enough to handle the environment and will go
    ;; extinct. For example, in an environment with few organisms,
    ;; a herbivorious species may go extinct if it can't 'select'
    ;; carnivorous traits fast enought to adapt to the conditions.

    set size initial-size
    set speed initial-speed
    set energy initial-energy
    setxy random-xcor random-ycor
    set breed-rate initial-breed-rate
    set herbivore-carnivore initial-herbivore-carnivore

    ;; the 'create-turtles' method operates as a loop that executes 'breednum' times.
    set breednum breednum - 1
  ]

  ;; all grass patches start fully grown.
   ask patches [
      set pcolor green
      set countdown grass-regrowth-time
  ]

  display-labels
  reset-ticks
end

to go

  if count turtles > 1000 [
    stop
  ]

  ;; each time interval, these are the behaviours that organisms exhibit:
  ask turtles [
    move                  ; 1. organisms move around the environment
    eat-grass             ; 2. organisms eat plant life
    eat-turtle            ; 3. organisms eat other organisms
    reproduce-turtles ;   ; 4. organisms reproduce
    death                 ; 5. organisms die
  ]

  ;; each time interval, these are the behaviours that grass patches exhibit:
  ask patches [
    grow-grass           ; 1. plants grow
  ]

  tick
  display-labels
end

;; organisms move in a random direction, at the organism's speed.
;; this costs the organism energy.
;; large organisms expend more energy moving than smaller organisms.
;; faster organisms expend more energy moving that slower organisms.
;; the environment itself imposes an 'overhead' on the movement.
;; for example, a human moving over difficult terrain expends more energy
;; to move the same distance compared to moving over easy terrain.

to move
  rt random 50
  lt random 50
  fd speed

  ; turtles lose energy as they move. larger and faster creatures require more energy to move.
  set energy energy - ( (((size / 100) - ((speed  / 100))) / 100) * move-overhead )
end

;; breed rate represents how aggressively a creature tries to breed vs conserving energy. R-selection vs K-selection
to reproduce-turtles  ;

  ;; breed-rate is governed by the organisms 'breed aggressiveness' attribute.
  ;; aggressively breeding organisms will breed even if they themselves have little energy.
  ;; cautiously breeding organisms will wait until they have a lot of energy before breeding.

  if energy > breed-rate [

    ; divide energy between parent and offspring
    set energy ((energy / 2) / 100) * (100 - breed-overhead)

    hatch 1 [

      ;; vary the offspring's size
      calculate-random-variation
      if size > abs random-variation [ ; don't let it get to minus numbers
        set size size + random-variation
      ]

       ;; Prevents creatures from evolving smaller than size 0.5
       ;; The model grinds to a halt if thousands of tiny creatures are formed (smaller than visible on screen)
       ;; Of course, it is quite realistic for an ecosystem to also include millions of microscopic creatures,
       ;; so the effect of this will have to remain unknown without a very powerful system capable of handling
       ;; that many agents to run these tests.

      if size < 0.5 [
         set size 0.5
       ]

      ;; vary the offspring's speed

      if speed > abs random-variation [ ; don't let speed get set to a minus number: the creature becomes fast!
        set speed speed - random-variation ;; otherwise big+fast things tend to just dominate everything
      ]

      ;; vary the offspring's breed rate
      ;; The variation value is multiplied by 10, as this trait has been modelled as rought out of 100.
      ;; Traits such as size and speed are roughly out of 10, so they are not multiplied.

      calculate-random-variation
      if breed-rate > abs random-variation * 10 [ ; don't let breed rate get set to a minus number
         set breed-rate (breed-rate + random-variation)
      ]

      ;; vary the offspring's herbivore-carnivore optimisation
      ;; The variation value is multiplied by 10, as this trait has been modelled as rought out of 100.
      ;; Traits such as size and speed are roughly out of 10, so they are not multiplied.

      calculate-random-variation
      if herbivore-carnivore > abs (random-variation * 10) [ ; don't let it get to minus numbers
         set herbivore-carnivore (herbivore-carnivore + random-variation * 10)
      ]

    ]
  ]
end

;; creates a random 'generational variation' value
;; for example, when a creature reproduces, its offspring can be slightly faster or slightly slower.
to calculate-random-variation

  ;; (1) generate the random value
  let change random-float generational-variation

  ;; (2) set the value to be positive or negative
  let coinflip random 2
  ifelse coinflip = 0
  [set random-variation 0 - change]
  [set random-variation change]
end

to eat-turtle

  ;; animals can only eat other animals on the same environment patch as them

  set flockmates other turtles in-radius 1
  set nearest-neighbor min-one-of flockmates [distance myself]

  if nearest-neighbor != nobody  [

    ;; organisms can only eat organisms that are smaller than themselves.
    ;; this is an abstraction of how effectively organisms can attack and defends themselves
    ;; while there are exceptions, this general rule holds throughout all ecosystems.

    if [size] of nearest-neighbor <= [size] of self [

      ;; animals do not eat animals of their own breed.
      ;; ideally, this trait should be 'evolved' and not defined from the beginning,
      ;; but this would greatly add to the complexity of the model.

      if [minibreed] of nearest-neighbor != [minibreed] of self [

        ;; receive the energy from eating the organism.
        ;; this is affected by two modifiers:
        ;; (a) carnivores receive more energy than herbivores from eating other organisms
        ;; (b) user defines a variable representing the inefficency value in how energy
        ;;     is absorbed by organisms: the act of eating and metabolising the energy
        ;;     never gives the animal the full energy value that existed in the meal.

        let maximum-energy [size] of nearest-neighbor
        let eat-mod  (maximum-energy / 100) * eat-overhead
        let herb-mod (eat-mod / 100)        * herbivore-carnivore
        set energy energy + herb-mod

        ;; if the nearby creature gets eaten, it dies!
        ask nearest-neighbor [ die ]
        set murder-deaths murder-deaths + 1

      ]
    ]
   ]

end

;; if animal runs out of energy, it has starved to death

to death
  if energy < 0 [
    set starve-deaths starve-deaths + 1
    die
  ]
end

;; display each creatures energy score using the switch 'show-energy?'

to display-labels
  ask turtles [ set label "" ]
  if show-energy? [
    ask turtles [ set label round energy ]
  ]
end

;; when grass is eaten, it starts a countdown until it regrows
;; this time is set by the user as 'grass-regrowth-time'

 to grow-grass
  if pcolor = brown [
    ifelse countdown <= 0
      [ set pcolor green
        set countdown grass-regrowth-time ]
      [ set countdown countdown - 1 ]
  ]
end

;; how an organism interacts with the grass in the environment.
;; more herbivorious animals are more efficient at gaining energy
;; from grass than carnivorous animals

to eat-grass  ; turtle procedure
  ; turtles eat grass, turn the patch brown
  if pcolor = green [
    set pcolor brown

    ;; high 'herbivore-carnivore'; value means animal is carnivore.
    ;; carnivores are less efficient at getting nutrients from grass

    let maximum-energy 1
    let eat-mod  (maximum-energy / 100) * eat-overhead
    let herb-mod (eat-mod / 100)        * (100 - herbivore-carnivore)
    set energy energy + herb-mod
  ]
end

@#$#@#$#@
GRAPHICS-WINDOW
190
10
703
524
-1
-1
9.902
1
14
1
1
1
0
1
1
1
-25
25
-25
25
1
1
1
ticks
30.0

SLIDER
5
405
185
438
eat-overhead
eat-overhead
0
100.0
72.9
0.01
1
NIL
HORIZONTAL

BUTTON
20
10
89
43
setup
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
90
10
165
43
go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SWITCH
20
45
165
78
show-energy?
show-energy?
1
1
-1000

SLIDER
5
205
185
238
initial-energy
initial-energy
0
10
0.573
0.001
1
NIL
HORIZONTAL

SLIDER
5
275
185
308
initial-number-of-breeds
initial-number-of-breeds
1
20
2.0
1
1
NIL
HORIZONTAL

SLIDER
5
135
185
168
initial-speed
initial-speed
0
1
0.86
0.01
1
NIL
HORIZONTAL

SLIDER
5
505
185
538
generational-variation
generational-variation
0
1
1.0
0.001
1
NIL
HORIZONTAL

PLOT
245
530
705
760
Creatures Per Breed
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
"breed2" 1.0 0 -16777216 true "" "plot count turtles with [ minibreed = 0 ]"
"pen-1" 1.0 0 -7500403 true "" "plot count turtles with [ minibreed = 1 ]"
"pen-2" 1.0 0 -2674135 true "" "plot count turtles with [ minibreed = 2 ]"
"pen-3" 1.0 0 -955883 true "" "plot count turtles with [ minibreed = 3 ]"
"pen-4" 1.0 0 -6459832 true "" "plot count turtles with [ minibreed = 4 ]"
"pen-5" 1.0 0 -1184463 true "" "plot count turtles with [ minibreed = 5 ]"
"pen-6" 1.0 0 -10899396 true "" "plot count turtles with [ minibreed = 6 ]"
"pen-7" 1.0 0 -13840069 true "" "plot count turtles with [ minibreed = 7 ]"
"pen-8" 1.0 0 -14835848 true "" "plot count turtles with [ minibreed = 8 ]"
"pen-9" 1.0 0 -11221820 true "" "plot count turtles with [ minibreed = 9 ]"
"pen-10" 1.0 0 -13791810 true "" "plot count turtles with [ minibreed = 10 ]"
"pen-11" 1.0 0 -13345367 true "" "plot count turtles with [ minibreed = 11 ]"
"pen-12" 1.0 0 -8630108 true "" "plot count turtles with [ minibreed = 12 ]"
"pen-13" 1.0 0 -5825686 true "" "plot count turtles with [ minibreed = 13 ]"
"pen-14" 1.0 0 -2064490 true "" "plot count turtles with [ minibreed = 14 ]"
"pen-15" 1.0 0 -16777216 true "" "plot count turtles with [ minibreed = 15 ]"
"pen-16" 1.0 0 -16777216 true "" "plot count turtles with [ minibreed = 16 ]"
"pen-17" 1.0 0 -16777216 true "" "plot count turtles with [ minibreed = 17 ]"
"pen-18" 1.0 0 -16777216 true "" "plot count turtles with [ minibreed = 18 ]"
"pen-19" 1.0 0 -16777216 true "" "plot count turtles with [ minibreed = 19 ]"
"pen-20" 1.0 0 -16777216 true "" "plot count turtles with [ minibreed = 20 ]"

SLIDER
5
100
185
133
initial-size
initial-size
0
2
2.0
0.1
1
NIL
HORIZONTAL

PLOT
715
13
1180
188
Size
NIL
NIL
0.0
0.1
0.0
0.1
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot mean [size] of turtles with [ minibreed = 4 ]"
"pen-1" 1.0 0 -7500403 true "" "plot mean [size] of turtles with [ minibreed = 1 ]"
"pen-2" 1.0 0 -2674135 true "" "plot mean [size] of turtles with [ minibreed = 2 ]"
"pen-3" 1.0 0 -955883 true "" "plot mean [size] of turtles with [ minibreed = 5 ]"
"pen-4" 1.0 0 -6459832 true "" "plot mean [size] of turtles with [ minibreed = 6 ]"
"pen-5" 1.0 0 -1184463 true "" "plot mean [size] of turtles with [ minibreed = 7 ]"
"pen-6" 1.0 0 -10899396 true "" "plot mean [size] of turtles with [ minibreed = 8 ]"
"pen-7" 1.0 0 -13840069 true "" "plot mean [size] of turtles with [ minibreed = 9 ]"
"pen-8" 1.0 0 -14835848 true "" "plot mean [size] of turtles with [ minibreed = 10 ]"
"pen-9" 1.0 0 -11221820 true "" "plot mean [size] of turtles with [ minibreed = 11 ]"
"pen-10" 1.0 0 -13791810 true "" "plot mean [size] of turtles with [ minibreed = 12 ]"
"pen-11" 1.0 0 -13345367 true "" "plot mean [size] of turtles with [ minibreed = 13 ]"
"pen-12" 1.0 0 -8630108 true "" "plot mean [size] of turtles with [ minibreed = 14 ]"
"pen-13" 1.0 0 -5825686 true "" "plot mean [size] of turtles with [ minibreed = 15 ]"
"pen-14" 1.0 0 -2064490 true "" "plot mean [size] of turtles with [ minibreed = 16 ]"
"pen-15" 1.0 0 -16777216 true "" "plot mean [size] of turtles with [ minibreed = 17 ]"
"pen-16" 1.0 0 -16777216 true "" "plot mean [size] of turtles with [ minibreed = 18 ]"
"pen-17" 1.0 0 -16777216 true "" "plot mean [size] of turtles with [ minibreed = 19 ]"
"pen-18" 1.0 0 -16777216 true "" "plot mean [size] of turtles with [ minibreed = 20 ]"
"pen-19" 1.0 0 -14439633 true "" "plot mean [size] of turtles with [ minibreed = 3 ]"
"pen-20" 1.0 0 -13791810 true "" "plot mean [size] of turtles with [ minibreed = 0 ]"

SLIDER
5
335
185
368
breed-overhead
breed-overhead
0
100
26.11
0.01
1
NIL
HORIZONTAL

SLIDER
5
170
185
203
initial-breed-rate
initial-breed-rate
0
100
6.0
1
1
NIL
HORIZONTAL

SLIDER
5
370
185
403
move-overhead
move-overhead
0
100
84.08
0.01
1
NIL
HORIZONTAL

SLIDER
5
240
185
273
initial-herbivore-carnivore
initial-herbivore-carnivore
0
100
50.0
1
1
NIL
HORIZONTAL

SLIDER
5
470
185
503
grass-regrowth-time
grass-regrowth-time
0
1000
885.0
1
1
NIL
HORIZONTAL

PLOT
715
188
1180
388
Herbivore - Carnivore (Carnivore = higher)
NIL
NIL
0.0
10.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot mean [herbivore-carnivore] of turtles with [ minibreed = 0 ]"
"pen-1" 1.0 0 -7500403 true "" "plot mean [herbivore-carnivore] of turtles with [ minibreed = 1 ]"
"pen-2" 1.0 0 -2674135 true "" "plot mean [herbivore-carnivore] of turtles with [ minibreed = 2 ]"
"pen-3" 1.0 0 -955883 true "" "plot mean [herbivore-carnivore] of turtles with [ minibreed = 3 ]"
"pen-4" 1.0 0 -6459832 true "" "plot mean [herbivore-carnivore] of turtles with [ minibreed = 4 ]"
"pen-5" 1.0 0 -1184463 true "" "plot mean [herbivore-carnivore] of turtles with [ minibreed = 5 ]"
"pen-6" 1.0 0 -10899396 true "" "plot mean [herbivore-carnivore] of turtles with [ minibreed = 6 ]"
"pen-7" 1.0 0 -13840069 true "" "plot mean [herbivore-carnivore] of turtles with [ minibreed = 7 ]"
"pen-8" 1.0 0 -14835848 true "" "plot mean [herbivore-carnivore] of turtles with [ minibreed = 8 ]"
"pen-9" 1.0 0 -11221820 true "" "plot mean [herbivore-carnivore] of turtles with [ minibreed = 9 ]"
"pen-10" 1.0 0 -13791810 true "" "plot mean [herbivore-carnivore] of turtles with [ minibreed = 10 ]"
"pen-11" 1.0 0 -13345367 true "" "plot mean [herbivore-carnivore] of turtles with [ minibreed = 11 ]"
"pen-12" 1.0 0 -8630108 true "" "plot mean [herbivore-carnivore] of turtles with [ minibreed = 12 ]"
"pen-13" 1.0 0 -5825686 true "" "plot mean [herbivore-carnivore] of turtles with [ minibreed = 13 ]"
"pen-14" 1.0 0 -2064490 true "" "plot mean [herbivore-carnivore] of turtles with [ minibreed = 14 ]"
"pen-15" 1.0 0 -16777216 true "" "plot mean [herbivore-carnivore] of turtles with [ minibreed = 15 ]"
"pen-16" 1.0 0 -16777216 true "" "plot mean [herbivore-carnivore] of turtles with [ minibreed = 16 ]"
"pen-17" 1.0 0 -16777216 true "" "plot mean [herbivore-carnivore] of turtles with [ minibreed = 17 ]"
"pen-18" 1.0 0 -16777216 true "" "plot mean [herbivore-carnivore] of turtles with [ minibreed = 18 ]"
"pen-19" 1.0 0 -16777216 true "" "plot mean [herbivore-carnivore] of turtles with [ minibreed = 19 ]"
"pen-20" 1.0 0 -16777216 true "" "plot mean [herbivore-carnivore] of turtles with [ minibreed = 20 ]"

PLOT
715
388
1180
583
Speed
NIL
NIL
0.0
10.0
0.0
0.01
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot mean [speed] of turtles with [ minibreed = 0 ]"
"pen-1" 1.0 0 -7500403 true "" "plot mean [speed] of turtles with [ minibreed = 1 ]"
"pen-2" 1.0 0 -2674135 true "" "plot mean [speed] of turtles with [ minibreed = 2 ]"
"pen-3" 1.0 0 -955883 true "" "plot mean [speed] of turtles with [ minibreed = 3 ]"
"pen-4" 1.0 0 -6459832 true "" "plot mean [speed] of turtles with [ minibreed = 4 ]"
"pen-5" 1.0 0 -1184463 true "" "plot mean [speed] of turtles with [ minibreed = 5 ]"
"pen-6" 1.0 0 -10899396 true "" "plot mean [speed] of turtles with [ minibreed = 6 ]"
"pen-7" 1.0 0 -13840069 true "" "plot mean [speed] of turtles with [ minibreed = 7 ]"
"pen-8" 1.0 0 -14835848 true "" "plot mean [speed] of turtles with [ minibreed = 8 ]"
"pen-9" 1.0 0 -11221820 true "" "plot mean [speed] of turtles with [ minibreed = 9 ]"
"pen-10" 1.0 0 -13791810 true "" "plot mean [speed] of turtles with [ minibreed = 10 ]"
"pen-11" 1.0 0 -13345367 true "" "plot mean [speed] of turtles with [ minibreed = 11 ]"
"pen-12" 1.0 0 -8630108 true "" "plot mean [speed] of turtles with [ minibreed = 12 ]"
"pen-13" 1.0 0 -5825686 true "" "plot mean [speed] of turtles with [ minibreed = 13 ]"
"pen-14" 1.0 0 -2064490 true "" "plot mean [speed] of turtles with [ minibreed = 14 ]"
"pen-15" 1.0 0 -16777216 true "" "plot mean [speed] of turtles with [ minibreed = 15 ]"
"pen-16" 1.0 0 -16777216 true "" "plot mean [speed] of turtles with [ minibreed = 16 ]"
"pen-17" 1.0 0 -16777216 true "" "plot mean [speed] of turtles with [ minibreed = 17 ]"
"pen-18" 1.0 0 -16777216 true "" "plot mean [speed] of turtles with [ minibreed = 18 ]"
"pen-19" 1.0 0 -16777216 true "" "plot mean [speed] of turtles with [ minibreed = 19 ]"
"pen-20" 1.0 0 -16777216 true "" "plot mean [speed] of turtles with [ minibreed = 20 ]"

PLOT
715
583
1180
758
Breed Rate
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
"default" 1.0 0 -16777216 true "" "plot mean [breed-rate] of turtles with [ minibreed = 0 ]"
"pen-1" 1.0 0 -7500403 true "" "plot mean [breed-rate] of turtles with [ minibreed = 1 ]"
"pen-2" 1.0 0 -2674135 true "" "plot mean [breed-rate] of turtles with [ minibreed = 2 ]"
"pen-3" 1.0 0 -955883 true "" "plot mean [breed-rate] of turtles with [ minibreed = 3 ]"
"pen-4" 1.0 0 -6459832 true "" "plot mean [breed-rate] of turtles with [ minibreed = 4 ]"
"pen-5" 1.0 0 -1184463 true "" "plot mean [breed-rate] of turtles with [ minibreed = 5 ]"
"pen-6" 1.0 0 -10899396 true "" "plot mean [breed-rate] of turtles with [ minibreed = 6 ]"
"pen-7" 1.0 0 -13840069 true "" "plot mean [breed-rate] of turtles with [ minibreed = 7 ]"
"pen-8" 1.0 0 -14835848 true "" "plot mean [breed-rate] of turtles with [ minibreed = 8 ]"
"pen-9" 1.0 0 -11221820 true "" "plot mean [breed-rate] of turtles with [ minibreed = 9 ]"
"pen-10" 1.0 0 -13791810 true "" "plot mean [breed-rate] of turtles with [ minibreed = 10 ]"
"pen-11" 1.0 0 -13345367 true "" "plot mean [breed-rate] of turtles with [ minibreed = 11 "
"pen-12" 1.0 0 -8630108 true "" "plot mean [breed-rate] of turtles with [ minibreed = 12 ]"
"pen-13" 1.0 0 -5825686 true "" "plot mean [breed-rate] of turtles with [ minibreed = 13 ]"
"pen-14" 1.0 0 -2064490 true "" "plot mean [breed-rate] of turtles with [ minibreed = 14 ]"
"pen-15" 1.0 0 -16777216 true "" "plot mean [breed-rate] of turtles with [ minibreed = 15 ]"
"pen-16" 1.0 0 -16777216 true "" "plot mean [breed-rate] of turtles with [ minibreed = 16 ]"
"pen-17" 1.0 0 -16777216 true "" "plot mean [breed-rate] of turtles with [ minibreed = 17 ]"
"pen-18" 1.0 0 -16777216 true "" "plot mean [breed-rate] of turtles with [ minibreed = 18 ]"
"pen-19" 1.0 0 -16777216 true "" "plot mean [breed-rate] of turtles with [ minibreed = 19 ]"
"pen-20" 1.0 0 -16777216 true "" "plot mean [breed-rate] of turtles with [ minibreed = 20 ]"

PLOT
30
595
235
760
Starve vs Murder
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
"Starve" 1.0 0 -16777216 true "" "plot starve-deaths"
"Murder" 1.0 0 -955883 true "" "plot murder-deaths"

TEXTBOX
50
85
200
103
Initial Conditions
11
0.0
1

TEXTBOX
40
320
190
338
Biology Conditions
11
0.0
1

TEXTBOX
25
450
175
468
Environment Conditions\n
11
0.0
1

@#$#@#$#@
## WHAT IS IT?



## HOW IT WORKS


## HOW TO USE IT

Parameters:
MODEL-VERSION: Whether we model sheep wolves and grass or just sheep and wolves
INITIAL-NUMBER-SHEEP: The initial size of sheep population
INITIAL-NUMBER-WOLVES: The initial size of wolf population
SHEEP-GAIN-FROM-FOOD: The amount of energy sheep get for every grass patch eaten (Note this is not used in the sheep-wolves model version)
WOLF-GAIN-FROM-FOOD: The amount of energy wolves get for every sheep eaten
SHEEP-REPRODUCE: The probability of a sheep reproducing at each time step
WOLF-REPRODUCE: The probability of a wolf reproducing at each time step
GRASS-REGROWTH-TIME: How long it takes for grass to regrow once it is eaten (Note this is not used in the sheep-wolves model version)
SHOW-ENERGY?: Whether or not to show the energy of each animal as a number

Notes:
- one unit of energy is deducted for every step a wolf takes
- when running the sheep-wolves-grass model version, one unit of energy is deducted for every step a sheep takes

There are three monitors to show the populations of the wolves, sheep and grass and a populations plot to display the population values over time.

If there are no wolves left and too many sheep, the model run stops.

## THINGS TO NOTICE

When running the sheep-wolves model variation, watch as the sheep and wolf populations fluctuate. Notice that increases and decreases in the sizes of each population are related. In what way are they related? What eventually happens?

In the sheep-wolves-grass model variation, notice the green line added to the population plot representing fluctuations in the amount of grass. How do the sizes of the three populations appear to relate now? What is the explanation for this?

Why do you suppose that some variations of the model might be stable while others are not?

## THINGS TO TRY

Try adjusting the parameters under various settings. How sensitive is the stability of the model to the particular parameters?

Can you find any parameters that generate a stable ecosystem in the sheep-wolves model variation?

Try running the sheep-wolves-grass model variation, but setting INITIAL-NUMBER-WOLVES to 0. This gives a stable ecosystem with only sheep and grass. Why might this be stable while the variation with only sheep and wolves is not?

Notice that under stable settings, the populations tend to fluctuate at a predictable pace. Can you find any parameters that will speed this up or slow it down?

## EXTENDING THE MODEL

There are a number ways to alter the model so that it will be stable with only wolves and sheep (no grass). Some will require new elements to be coded in or existing behaviors to be changed. Can you develop such a version?

Try changing the reproduction rules -- for example, what would happen if reproduction depended on energy rather than being determined by a fixed probability?

Can you modify the model so the sheep will flock?

Can you modify the model so that wolves actively chase sheep?

## NETLOGO FEATURES

Note the use of breeds to model two different kinds of "turtles": wolves and sheep. Note the use of patches to model grass.

Note use of the ONE-OF agentset reporter to select a random sheep to be eaten by a wolf.
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
Polygon -7500403 true true 136 61 129 46 144 30 119 45 124 60 114 82 97 37 132 10 93 36 111 84 127 105 172 105 189 84 208 35 171 11 202 35 204 37 186 82 177 60 180 44 159 32 170 44 165 60
Polygon -7500403 true true 150 95 135 103 139 117 125 149 137 180 135 196 150 204 166 195 161 180 174 150 158 116 164 102
Polygon -7500403 true true 149 186 128 197 114 232 134 270 149 282 166 270 185 232 171 195 149 186
Polygon -7500403 true true 225 66 230 107 159 122 161 127 234 111 236 106
Polygon -7500403 true true 78 58 99 116 139 123 137 128 95 119
Polygon -7500403 true true 48 103 90 147 129 147 130 151 86 151
Polygon -7500403 true true 65 224 92 171 134 160 135 164 95 175
Polygon -7500403 true true 235 222 210 170 163 162 161 166 208 174
Polygon -7500403 true true 249 107 211 147 168 147 168 150 213 150

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

bird
false
0
Polygon -7500403 true true 135 165 90 270 120 300 180 300 210 270 165 165
Rectangle -7500403 true true 120 105 180 237
Polygon -7500403 true true 135 105 120 75 105 45 121 6 167 8 207 25 257 46 180 75 165 105
Circle -16777216 true false 128 21 42
Polygon -7500403 true true 163 116 194 92 212 86 230 86 250 90 265 98 279 111 290 126 296 143 298 158 298 166 296 183 286 204 272 219 259 227 235 240 241 223 250 207 251 192 245 180 232 168 216 162 200 162 186 166 175 173 171 180
Polygon -7500403 true true 137 116 106 92 88 86 70 86 50 90 35 98 21 111 10 126 4 143 2 158 2 166 4 183 14 204 28 219 41 227 65 240 59 223 50 207 49 192 55 180 68 168 84 162 100 162 114 166 125 173 129 180

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

cow skull
false
0
Polygon -7500403 true true 150 90 75 105 60 150 75 210 105 285 195 285 225 210 240 150 225 105
Polygon -16777216 true false 150 150 90 195 90 150
Polygon -16777216 true false 150 150 210 195 210 150
Polygon -16777216 true false 105 285 135 270 150 285 165 270 195 285
Polygon -7500403 true true 240 150 263 143 278 126 287 102 287 79 280 53 273 38 261 25 246 15 227 8 241 26 253 46 258 68 257 96 246 116 229 126
Polygon -7500403 true true 60 150 37 143 22 126 13 102 13 79 20 53 27 38 39 25 54 15 73 8 59 26 47 46 42 68 43 96 54 116 71 126

cylinder
false
0
Circle -7500403 true true 0 0 300

dog
false
0
Polygon -7500403 true true 300 165 300 195 270 210 183 204 180 240 165 270 165 300 120 300 0 240 45 165 75 90 75 45 105 15 135 45 165 45 180 15 225 15 255 30 225 30 210 60 225 90 225 105
Polygon -16777216 true false 0 240 120 300 165 300 165 285 120 285 10 221
Line -16777216 false 210 60 180 45
Line -16777216 false 90 45 90 90
Line -16777216 false 90 90 105 105
Line -16777216 false 105 105 135 60
Line -16777216 false 90 45 135 60
Line -16777216 false 135 60 135 45
Line -16777216 false 181 203 151 203
Line -16777216 false 150 201 105 171
Circle -16777216 true false 171 88 34
Circle -16777216 false false 261 162 30

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

ghost
false
0
Polygon -7500403 true true 30 165 13 164 -2 149 0 135 -2 119 0 105 15 75 30 75 58 104 43 119 43 134 58 134 73 134 88 104 73 44 78 14 103 -1 193 -1 223 29 208 89 208 119 238 134 253 119 240 105 238 89 240 75 255 60 270 60 283 74 300 90 298 104 298 119 300 135 285 135 285 150 268 164 238 179 208 164 208 194 238 209 253 224 268 239 268 269 238 299 178 299 148 284 103 269 58 284 43 299 58 269 103 254 148 254 193 254 163 239 118 209 88 179 73 179 58 164
Line -16777216 false 189 253 215 253
Circle -16777216 true false 102 30 30
Polygon -16777216 true false 165 105 135 105 120 120 105 105 135 75 165 75 195 105 180 120
Circle -16777216 true false 160 30 30

hawk
true
0
Polygon -7500403 true true 151 170 136 170 123 229 143 244 156 244 179 229 166 170
Polygon -16777216 true false 152 154 137 154 125 213 140 229 159 229 179 214 167 154
Polygon -7500403 true true 151 140 136 140 126 202 139 214 159 214 176 200 166 140
Polygon -16777216 true false 151 125 134 124 128 188 140 198 161 197 174 188 166 125
Polygon -7500403 true true 152 86 227 72 286 97 272 101 294 117 276 118 287 131 270 131 278 141 264 138 267 145 228 150 153 147
Polygon -7500403 true true 160 74 159 61 149 54 130 53 139 62 133 81 127 113 129 149 134 177 150 206 168 179 172 147 169 111
Circle -16777216 true false 144 55 7
Polygon -16777216 true false 129 53 135 58 139 54
Polygon -7500403 true true 148 86 73 72 14 97 28 101 6 117 24 118 13 131 30 131 22 141 36 138 33 145 72 150 147 147

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

rabbit
false
0
Polygon -7500403 true true 61 150 76 180 91 195 103 214 91 240 76 255 61 270 76 270 106 255 132 209 151 210 181 210 211 240 196 255 181 255 166 247 151 255 166 270 211 270 241 255 240 210 270 225 285 165 256 135 226 105 166 90 91 105
Polygon -7500403 true true 75 164 94 104 70 82 45 89 19 104 4 149 19 164 37 162 59 153
Polygon -7500403 true true 64 98 96 87 138 26 130 15 97 36 54 86
Polygon -7500403 true true 49 89 57 47 78 4 89 20 70 88
Circle -16777216 true false 37 103 16
Line -16777216 false 44 150 104 150
Line -16777216 false 39 158 84 175
Line -16777216 false 29 159 57 195
Polygon -5825686 true false 0 150 15 165 15 150
Polygon -5825686 true false 76 90 97 47 130 32
Line -16777216 false 180 210 165 180
Line -16777216 false 165 180 180 165
Line -16777216 false 180 165 225 165
Line -16777216 false 180 210 210 240

shark
false
0
Polygon -7500403 true true 283 153 288 149 271 146 301 145 300 138 247 119 190 107 104 117 54 133 39 134 10 99 9 112 19 142 9 175 10 185 40 158 69 154 64 164 80 161 86 156 132 160 209 164
Polygon -7500403 true true 199 161 152 166 137 164 169 154
Polygon -7500403 true true 188 108 172 83 160 74 156 76 159 97 153 112
Circle -16777216 true false 256 129 12
Line -16777216 false 222 134 222 150
Line -16777216 false 217 134 217 150
Line -16777216 false 212 134 212 150
Polygon -7500403 true true 78 125 62 118 63 130
Polygon -7500403 true true 121 157 105 161 101 156 106 152

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
Polygon -7500403 true true 134 255 104 240 96 210 98 196 114 171 134 150 119 135 119 120 134 105 164 105 179 120 179 135 164 150 185 173 199 195 203 210 194 240 164 255
Line -7500403 true 167 109 170 90
Line -7500403 true 170 91 156 88
Line -7500403 true 130 91 144 88
Line -7500403 true 133 109 130 90
Polygon -7500403 true true 167 117 207 102 216 71 227 27 227 72 212 117 167 132
Polygon -7500403 true true 164 210 158 194 195 195 225 210 195 285 240 210 210 180 164 180
Polygon -7500403 true true 136 210 142 194 105 195 75 210 105 285 60 210 90 180 136 180
Polygon -7500403 true true 133 117 93 102 84 71 73 27 73 72 88 117 133 132
Polygon -7500403 true true 163 140 214 129 234 114 255 74 242 126 216 143 164 152
Polygon -7500403 true true 161 183 203 167 239 180 268 239 249 171 202 153 163 162
Polygon -7500403 true true 137 140 86 129 66 114 45 74 58 126 84 143 136 152
Polygon -7500403 true true 139 183 97 167 61 180 32 239 51 171 98 153 137 162

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

wolf 2
false
0
Rectangle -7500403 true true 195 106 285 150
Rectangle -7500403 true true 195 90 255 105
Polygon -7500403 true true 240 90 217 44 196 90
Polygon -16777216 true false 234 89 218 59 203 89
Rectangle -1 true false 240 93 252 105
Rectangle -16777216 true false 242 96 249 104
Rectangle -16777216 true false 241 125 285 139
Polygon -1 true false 285 125 277 138 269 125
Polygon -1 true false 269 140 262 125 256 140
Rectangle -7500403 true true 45 120 195 195
Rectangle -7500403 true true 45 114 185 120
Rectangle -7500403 true true 165 195 180 270
Rectangle -7500403 true true 60 195 75 270
Polygon -7500403 true true 45 105 15 30 15 75 45 150 60 120

wolf 3
false
0
Polygon -7500403 true true 105 180 75 180 45 75 45 0 105 45 195 45 255 0 255 75 225 180 195 180 165 300 135 300 105 180 75 180
Polygon -16777216 true false 225 90 210 135 150 90
Polygon -16777216 true false 75 90 90 135 150 90

wolf 4
false
0
Polygon -7500403 true true 105 75 105 45 45 0 30 45 45 60 60 90
Polygon -7500403 true true 45 165 30 135 45 120 15 105 60 75 105 60 180 60 240 75 285 105 255 120 270 135 255 165 270 180 255 195 255 210 240 195 195 225 210 255 180 300 120 300 90 255 105 225 60 195 45 210 45 195 30 180
Polygon -16777216 true false 120 300 135 285 120 270 120 255 180 255 180 270 165 285 180 300
Polygon -16777216 true false 240 135 180 165 180 135
Polygon -16777216 true false 60 135 120 165 120 135
Polygon -7500403 true true 195 75 195 45 255 0 270 45 255 60 240 90
Polygon -16777216 true false 225 75 210 60 210 45 255 15 255 45 225 60
Polygon -16777216 true false 75 75 90 60 90 45 45 15 45 45 75 60

wolf 5
false
0
Polygon -7500403 true true 135 285 165 285 270 90 30 90 135 285
Polygon -7500403 true true 270 90 225 15 180 90
Polygon -7500403 true true 30 90 75 15 120 90
Polygon -1 true false 225 150 180 195 165 165
Polygon -1 true false 75 150 120 195 135 165
Polygon -1 true false 135 285 135 255 150 240 165 255 165 285

wolf 6
false
0
Polygon -7500403 true true 105 75 105 45 45 0 30 45 45 60 60 90
Polygon -7500403 true true 45 165 30 135 45 120 15 105 60 75 105 60 180 60 240 75 285 105 255 120 270 135 255 165 270 180 255 195 255 210 240 195 195 225 210 255 180 300 120 300 90 255 105 225 60 195 45 210 45 195 30 180
Polygon -16777216 true false 120 300 135 285 120 270 120 255 180 255 180 270 165 285 180 300
Polygon -7500403 true true 195 75 195 45 255 0 270 45 255 60 240 90
Polygon -16777216 true false 225 75 210 60 210 45 255 15 255 45 225 60
Polygon -16777216 true false 75 75 90 60 90 45 45 15 45 45 75 60
Circle -16777216 true false 88 118 32
Circle -16777216 true false 178 118 32

wolf 7
false
0
Circle -16777216 true false 183 138 24
Circle -16777216 true false 93 138 24
Polygon -7500403 true true 30 105 30 150 90 195 120 270 120 300 180 300 180 270 210 195 270 150 270 105 210 75 90 75
Polygon -7500403 true true 255 105 285 60 255 0 210 45 195 75
Polygon -7500403 true true 45 105 15 60 45 0 90 45 105 75
Circle -16777216 true false 90 135 30
Circle -16777216 true false 180 135 30
Polygon -16777216 true false 120 300 150 255 180 300

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.4
@#$#@#$#@
set model-version "sheep-wolves-grass"
set show-energy? false
setup
repeat 75 [ go ]
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="1A" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>mean [size] of turtles with [ minibreed = 1 ]</metric>
    <metric>mean [size] of turtles with [ minibreed = 2 ]</metric>
    <enumeratedValueSet variable="initial-herbivore-carnivore">
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="1B" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1500"/>
    <metric>mean [herbivore-carnivore] of turtles with [ minibreed = 1 ]</metric>
    <metric>mean [herbivore-carnivore] of turtles with [ minibreed = 2 ]</metric>
    <metric>mean [size] of turtles with [ minibreed = 1 ]</metric>
    <metric>mean [size] of turtles with [ minibreed = 2 ]</metric>
    <metric>mean [speed] of turtles with [ minibreed = 1 ]</metric>
    <metric>mean [speed] of turtles with [ minibreed = 2 ]</metric>
    <metric>mean [breed-rate] of turtles with [ minibreed = 1 ]</metric>
    <metric>mean [breed-rate] of turtles with [ minibreed = 2 ]</metric>
    <enumeratedValueSet variable="initial-herbivore-carnivore">
      <value value="0"/>
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
      <value value="90"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-speed">
      <value value="0.86"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-energy">
      <value value="0.573"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-energy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="variation">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eat-vision">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-size">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eat-overhead">
      <value value="72.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-breeds">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-breed-rate">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="breed-overhead">
      <value value="26.11"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-overhead">
      <value value="84.08"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="885"/>
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
1
@#$#@#$#@
