; - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
;                 THE FRIENDSHIP FIELD                  ;
; - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
; by Chrisja van de Kieft & Eva Timmer                  ;
; Agent-Based Modeling of Complex Adaptive Systems      ;
; INF34806                                              ;
; February/March 2022                                   ;
; Updated August 2022                                   ;
; - - - - - - - - - - - - - - - - - - - - - - - - - - - ;



; = = = = = SET UP = = = = = ;

breed [ humans human ]
links-own [
  relation
  resemblance
]
extensions [rnd]

humans-own
[
  extraversion                ;; -1 (very introvert) to 1 (very extravert)
  status                      ;; 0 (low status) to 1 (high status)
  kindness                    ;; 0 (unkind) to 1 (very kind)
  appeal                      ;; 0 (not appealing) to 1 (very appealing)
  characteristics-list        ;; list of 1 or 0s indicating that an abstract character is present/absent
  social-battery              ;; 0 - 100 energy to socialize
  candidate                   ;; possible agent to mate with
  mate                        ;; other agent the person mates with
  is-available?               ;; whether the agent is available to meet
  friends-counter             ;; the number of friends an agent has
]

globals                       ;; including those defined in the Interface
[
  min-relation                ;; minimal value for relation
  max-relation                ;; maximal value for relation
  min-prob                    ;; minimal raw probability for agent to be chosen as candidate
  max-prob                    ;; maximal raw probability for agent to be chosen as candidate
  volatility                  ;; importance of the conferral in status changing
  chars-weight                ;; importance of the percentage of similar characteristics in interact value
  openness-weight             ;; importance of the openness in interact value
  ;number-of-humans           ;; number of humans in the field
  ;sb-threshold               ;; amount of social battery that is needed to interact
  ;friendship-threshold       ;; when a relation is considered good enough to be a friendship
  ;introversion-weight        ;; how heavy introversion weighs to have the energy to socially interact
  ;field-battery-constant     ;; how heavy chilling on the field affects social battery
  ;interact-battery-constant  ;; how heavy social interaction affects social battery
  ;status-weight              ;; how important the status is in the status conferral
  ;personality-weight         ;; how important the personality is in the status conferral
  ;relation-weight            ;; how important the current relationship is in the status conferral
  ;number-of-characteristics  ;; number of characteristics the agents have
  extrav-chillers             ;; total number of extraverts chilling on the field
  introv-chillers             ;; total number of introverts chilling on the field
]


to setup
  clear-all
  setup-humans
  set-parameters
  setup-patches
  reset-ticks
end

to set-parameters
  set min-relation 0          ;; relation can't be negative (enemies)
  set max-relation 1          ;; relation can't be higher than 1 (ultimate friends)
  set min-prob 0.1            ;; each agent has at least a probability of 0.1 to be chosen as candidate
  set max-prob 0.6            ;; each agent has at most a probability of 0.6 to be chosen as candidate
  set volatility 0.1          ;; status conferral weighs for 0.1 in changing the status
  set chars-weight 1.5        ;; similar characteristics weighs 1.5 times the percentage
  set openness-weight 2       ;; openness accounts accounts for a great deal of the interact value
  set extrav-chillers 0       ;; start value is 0
  set introv-chillers 0       ;; start value is 0
end

to setup-humans
  create-humans number-of-humans [
    ;;;; Visualization
    setxy random-pxcor random-pycor
    set shape "person"

    ;;;; Parameters Setting
    set extraversion one-of [ -1.0 -0.9 -0.8 -0.7 -0.6 -0.5 -0.4 -0.3 -0.2 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0]
    set color scale-color (blue - 3) extraversion -2 1
    set status random-float 1.0
    set kindness random-float 1.0
    set appeal random-float 1.0
    set characteristics-list n-values number-of-characteristics [ random 2 ]
    set social-battery sb-threshold
    set mate nobody
    set is-available? true
    set candidate -1
  ]

  ;;;; Initiate Relations
  ask humans [
    create-links-with other humans [
      set relation 0 set color green set hidden? relation < friendship-threshold           ;; initial relations are non-existent
    ]
  ]
end

to setup-patches
  ask patches [ set pcolor black]
end


; = = = = =  GO PROCEDURE  = = = = = ;

to go
  ;;;; Meet
  ask humans [
    let want-to-meet calculate-wanting social-battery extraversion                         ;; Calculate probability that agent wants to meet someone
    let not-want-to-meet 1 - want-to-meet
    let probs (list want-to-meet not-want-to-meet)
    set is-available? first rnd:weighted-one-of-list (map list [true false] probs) last    ;; Introduce randomicity into being available
  ]

  meet                                                                                     ;; Agent tries to find a mate

  ;;;; Interact or not
  ask humans [
    ifelse mate != nobody [                                                                ;; Check if the agent became mates with someone
      mates-interact                                                                       ;; If agent has mate, they interact
    ] [
      field-social-battery                                                                 ;; If agent hasn't got a mate, it goes chilling on the field
    ]
  ]

  count-chillers                                                                           ;; Count number of introverts & extraverts chilling on the field

  ;;;; Unmate
  un-mate
  decay-relations                                                                          ;; Relations constantly decay when no interactions between agents occur
  count-friends                                                                            ;; Count the number of friends an agent has

  tick
end

to mates-interact
  ;;;; Interact
  let conferral determine-conferral
  let interaction-value interact conferral

  ;;;; Update
  update-relationship interaction-value
  update-status conferral
  interact-social-battery
end


to meet
  ask humans [
    if is-available? [                                                                     ;; Check whether agent is available
      ;;;; Sort the candidates
      let candidates sort ([who] of other turtles with [is-available?] )                   ;; the 'who' can be used for link retrieval

      let number-of-candidates length candidates
      if number-of-candidates > 0 [                                                        ;; If at least 1 other agent is available

        ;;;; Retrieve raw probabilities
        let raw-probabilities []
        foreach candidates [ [c] ->
          let prob ( [ relation ] of link who c ) + min-prob                               ;; each raw probability is at least min-prob
          if prob >= max-prob [ set prob max-prob ]                                        ;; each raw probability is at most max-prob
          set raw-probabilities lput ( prob ) raw-probabilities
        ]

        ;;;; Normalize probabilities
        let norm-probabilities []
        foreach raw-probabilities [ [p] ->
          set norm-probabilities lput ( p / sum raw-probabilities ) norm-probabilities
        ]

        ;;;; Pick candidate
        set candidate first rnd:weighted-one-of-list (map list candidates raw-probabilities) last
        set candidate human candidate                                                      ;; because candidate is still a number rather than an agent

        ifelse [relation] of link who [who] of candidate >= friendship-threshold [         ;; Check if the candidate is already a friend
          set mate candidate
          ask candidate [ set mate myself ]
          ask candidate [ set is-available? false ]
          set is-available? false
        ] [
          ifelse extraversion > 0 [                                                        ;; If the mate is note yet a friend, check extraversion
            set mate candidate
            ask candidate [ set mate myself ]
            ask candidate [ set is-available? false ]
            set is-available? false
          ] [                                                                              ;; If agent is introvert
            ifelse social-battery + extraversion * introversion-weight >= sb-threshold [   ;; If social battery is high enough, agent meets someone new
              set mate candidate
              ask candidate [ set mate myself ]
              ask candidate [ set is-available? false ]
              set is-available? false
            ] [
              set mate nobody                                                              ;; If social battery is too low, agent goes chilling on the field
            ]
          ]
        ]
      ]
    ]
  ]
end


to-report calculate-difference-value [soc-bat max-sb min-sb]
  report (1 - abs(soc-bat - sb-threshold) / (max-sb - min-sb) ) / 2                        ;; Calculate relative difference between social battery and threshold
end

to-report calculate-wanting [ soc-bat extrav ]
  let relative-difference 0
  ifelse soc-bat > sb-threshold [                                                          ;; If agent has "high" social battery
    set relative-difference calculate-difference-value soc-bat 100 sb-threshold
    ifelse extrav > 0 [
      set relative-difference relative-difference                                          ;; Extraverts with enough social battery don't really need to meet (0 to 0.5)
    ] [
      set relative-difference 1 - relative-difference                                      ;; Introverts with enough social battery do want to meet (0.5 to 1)
    ]
  ] [                                                                                      ;; If agent has "low" social battery
    set relative-difference calculate-difference-value soc-bat sb-threshold 0
    ifelse extrav > 0 [
      set relative-difference 1 - relative-difference                                      ;; Extraverts with little social energy do want to meet (0.5 to 1)
    ] [
      set relative-difference relative-difference                                          ;; Introverts with little social energy want to not meet (0 to 0.5)
    ]
  ]
  let wanting-to-meet relative-difference + extrav / 2                                     ;; Increase the wanting to meet based on the agent's extraversion

  ;;;; Bound result within 0 & 1
  ifelse wanting-to-meet > 1 [
    set wanting-to-meet 1
  ] [
    if wanting-to-meet < 0 [
      set wanting-to-meet 0
    ]
  ]
  report wanting-to-meet
end


to-report determine-conferral
  ;;;; Balance all the weights to sum up to 1
  let total-weights precision ( status-weight + personality-weight + relation-weight ) 5
  if total-weights != 0 [
    set status-weight precision ( status-weight / total-weights ) 2
    set personality-weight precision ( personality-weight / total-weights ) 2
    set relation-weight precision ( relation-weight / total-weights ) 2
  ]

  ;;;; Calculate Status Conferral
  report ( (status-weight * [status] of mate)
    + (personality-weight * (kindness + [appeal] of mate) / 2)
    + ([relation] of link who [who] of mate * relation-weight))
end

to-report interact [status-conferral]
  ;;;; Calculate % of agreeing characteristics
  let counter 0
  (foreach characteristics-list [characteristics-list] of mate [
    [ a b ] -> if a = b [ set counter counter + 1 ] ] )
  let percentage counter / length characteristics-list
  ask link who [ who ] of mate [ set resemblance percentage ]

  ;;;; Calculate openness
  let openness 0
  ifelse extraversion > 0 [                                                                ;; If agent is extravert, it is open despite the not good relation
    set openness extraversion * ( 1 - [relation] of link who [who] of mate )
  ] [                                                                                      ;; If agent is introvert, the better the relation the opener the agent
    set openness extraversion * ( - [ relation ] of link who [ who ] of mate )
  ]

  ;;;; Make interaction value
  report (percentage * chars-weight + openness * openness-weight  + status-conferral) / 7
end

to update-status [ status-conferral ]
  ask mate [
    set status (status + (status-conferral - status) * volatility)                         ;; Change the status of the mate using the Status Conferral
  ]
end

to bound-social-battery [ new-battery ]
  ifelse new-battery <= 0 [
    set social-battery 0
  ] [
    ifelse new-battery >= 100 [
      set social-battery 100
    ] [
      set social-battery new-battery
    ]
  ]
end

to interact-social-battery
  let new-social-battery social-battery + extraversion * interact-battery-constant         ;; Social battery recharges (extraverts) or exhausts (introverts) while interacting

  ;;;; Social Battery within boundaries 0 - 100
  bound-social-battery new-social-battery
end

to update-relationship [ interaction ]
  ;;;; Update the relationship with the interaction value
  ask link who [who] of mate [
    let new-relation relation + interaction
    ifelse new-relation > max-relation [
      set relation max-relation
    ] [
      set relation new-relation
    ]
  ]

  ;;;; When agents can adapt characteristics
  if adaptive-characteristics? [
    ask humans [
      if mate != nobody [
        if ( [ relation ] of link who [who] of mate >= friendship-threshold ) [
          if status < [ status ] of mate [                                                 ;; Adopt a characteristic of the agent with the most status
            let changed-characteristic false
            let counter 0
            (foreach characteristics-list [ characteristics-list ] of mate [
              [ own-c mate-c ] ->
              if not changed-characteristic [
                if own-c != mate-c [
                  set characteristics-list replace-item counter characteristics-list mate-c
                  set changed-characteristic true
                ]
              ]
              set counter counter + 1
            ])
          ]
          ask link who [ who ] of mate [
            if resemblance < 1 [
              set resemblance resemblance + 1 / number-of-characteristics                  ;; Update Resemblance
            ]
          ]
        ]
      ]
    ]
  ]
end

to un-mate
  ask links [ set hidden? relation < friendship-threshold ]                                ;; Links become visible when agents are friends
  ask humans [
    set mate nobody                                                                        ;; Agents lose their mate
  ]
end

to field-social-battery
  let new-social-battery social-battery - field-battery-constant * extraversion            ;; Social battery recharges (introverts) or exhausts (extroverts) while chilling on field

  ;;;; Social Battery within boundaries 0 - 100
  bound-social-battery new-social-battery
end

to decay-relations
  ask links [                                                                              ;; Each tick every relation decays with a constant, dependent on how good the relation was
    let new-relation ( relation - ( 1.1 - relation ) * 0.05 )
    ifelse new-relation > min-relation [
      set relation new-relation
    ] [
      set relation min-relation
    ]
  ]
end

to count-friends
  ask humans [                                                                             ;; Count the number of friends for each agent
    set friends-counter 0
    (foreach [who] of other turtles [
      [ w ] -> if [ relation ] of link who w > friendship-threshold [
        set friends-counter friends-counter + 1 ] ] )
  ]
end


; = = = = =  PLOT  = = = = = ;

;;;; Procedure for Plot "Number of Distinct Characteristics"
to-report count-distinct-characteristics
  let all-lists []
  let number-distinct 0
  ask humans [
    if not member? characteristics-list all-lists [
      set number-distinct number-distinct + 1
      set all-lists lput (characteristics-list) all-lists
    ]
  ]
  report number-distinct
end

;;;; Procedure for Monitor "% COF"
to count-chillers
  set extrav-chillers extrav-chillers + count turtles with [ extraversion > 0 and mate = nobody ]
  set introv-chillers introv-chillers + count turtles with [ extraversion < 0 and mate = nobody ]
end

;;;; Procedure for Output on association between status and number of friends
to generateOutput
  output-print [ status ] of turtles
  output-print [ friends-counter ] of turtles
end
@#$#@#$#@
GRAPHICS-WINDOW
197
20
634
458
-1
-1
13.0
1
10
1
1
1
0
1
1
1
-16
16
-16
16
1
1
1
ticks
20.0

BUTTON
16
21
95
57
NIL
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

SLIDER
1064
79
1236
112
sb-threshold
sb-threshold
0
100
60.0
1
1
%
HORIZONTAL

SLIDER
1064
288
1236
321
status-weight
status-weight
0
1
0.33
0.05
1
NIL
HORIZONTAL

SLIDER
1064
331
1236
364
personality-weight
personality-weight
0
1
0.33
0.05
1
NIL
HORIZONTAL

SLIDER
1064
373
1236
406
relation-weight
relation-weight
0
1
0.33
0.05
1
NIL
HORIZONTAL

SLIDER
1064
120
1236
153
friendship-threshold
friendship-threshold
0
1
0.6
0.1
1
NIL
HORIZONTAL

SLIDER
1064
162
1236
195
introversion-weight
introversion-weight
0
40
20.0
1
1
NIL
HORIZONTAL

SLIDER
1063
205
1235
238
field-battery-constant
field-battery-constant
0
40
30.0
1
1
NIL
HORIZONTAL

BUTTON
16
66
183
99
go once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
1064
247
1237
280
interact-battery-constant
interact-battery-constant
0
30
10.0
1
1
NIL
HORIZONTAL

SLIDER
1063
36
1235
69
number-of-humans
number-of-humans
2
100
60.0
1
1
NIL
HORIZONTAL

BUTTON
110
21
184
57
NIL
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

PLOT
651
32
841
176
Social Batteries
NIL
NIL
-10.0
120.0
0.0
6.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [ social-battery ] of humans"
"pen-1" 1.0 0 -2674135 true "" "plot-pen-reset\nplotxy sb-threshold 0\nplotxy sb-threshold plot-y-max\n"

PLOT
15
208
183
328
Extraversion Levels
NIL
NIL
-1.0
1.0
0.0
10.0
true
false
"" ""
PENS
"default" 0.05 1 -16777216 true "" "histogram [ extraversion ] of humans"

PLOT
849
32
1045
176
Number of Friendships
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
"default" 1.0 0 -16777216 true "" "let counter 0\n(foreach [ relation ] of links [\n     [ a ] -> if a > friendship-threshold [ \n     set counter counter + 1 ] ] )\nplot counter"

TEXTBOX
15
188
165
210
Static Statistics
15
0.0
1

TEXTBOX
653
10
803
29
Dynamic Statistics
15
0.0
1

TEXTBOX
1062
10
1212
29
Variables
15
0.0
1

PLOT
850
185
1046
326
Relationship Total Values
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
"default" 1.0 0 -16777216 true "" "plot sum [ relation ] of links"

PLOT
652
185
841
326
Friendship Distribution
NIL
NIL
0.0
30.0
0.0
10.0
true
false
"set-plot-x-range 0 min ( list number-of-humans 30)\n" ""
PENS
"default" 1.0 1 -13345367 true "" "histogram [ friends-counter ] of turtles"

SWITCH
15
151
181
184
adaptive-characteristics?
adaptive-characteristics?
1
1
-1000

PLOT
652
336
841
459
Number of Distinct Characteristics
NIL
NIL
0.0
10.0
0.0
15.0
true
false
";let combinations 2 ^ number-of-characteristics\n;let max-x min [ combinations number-of-humans ] \n;set-plot-y-range 0 number-of-humans" ""
PENS
"default" 1.0 0 -5298144 true "" "plotxy 0 1\nplotxy plot-x-max 1"
"pen-1" 1.0 0 -16777216 true "" "plot count-distinct-characteristics"

SLIDER
1065
415
1236
448
number-of-characteristics
number-of-characteristics
0
20
8.0
1
1
NIL
HORIZONTAL

PLOT
850
336
1047
459
Extraversion VS #Friends
Extraversion
Number of Friends
-1.0
1.0
0.0
10.0
true
false
"" ""
PENS
"pen-0" 0.1 2 -14333415 true "" "clear-plot\nask turtles [ plotxy extraversion friends-counter ]"

BUTTON
15
108
182
141
go for a year
setup\nlet day 1\nwhile [ day <= 365 * 3 ] [\ngo \nset day day + 1 \n]\ngenerateOutput
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
128
334
178
379
Extrav
sum [ friends-counter ] of turtles with [ extraversion > 0 ] / count turtles with [ extraversion > 0 ]
1
1
11

MONITOR
68
334
118
379
 Introv
sum [ friends-counter ] of turtles with [ extraversion < 0 ] / count turtles with [ extraversion < 0 ]
1
1
11

TEXTBOX
16
342
62
375
Average #Friends
11
0.0
1

MONITOR
129
384
179
429
Extrav
mean [ social-battery ] of turtles with [ extraversion > 0 ]
1
1
11

MONITOR
68
383
118
428
Introv
mean [ social-battery ] of turtles with [ extraversion < 0 ]
1
1
11

TEXTBOX
17
390
59
427
Social Battery 
11
0.0
1

MONITOR
129
433
179
478
Extrav
extrav-chillers / ticks / count turtles with [ extraversion > 0 ] * 100
1
1
11

TEXTBOX
19
439
60
464
% Chill on Field
11
0.0
1

MONITOR
68
433
118
478
Introv
introv-chillers / ticks / count turtles with [ extraversion < 0 ] * 100
1
1
11

@#$#@#$#@
## WHAT IS IT?

The Friendship Field model tries to simulate friendship formation in first year bachelor students in Wageningen using Extraversion, Resemblance, Status and Social Battery. 

## HOW IT WORKS

The agents have certain attributes (extraversion, characteristics, status, etc.), meet each other and based on their interaction (influenced by their attributes) they can build towards their relation and become friends. 

## HOW TO USE IT

Set a number of humans you want in the field, optionally adjust some parameters and run.

## THINGS TO NOTICE

When do links form and between whom. 

## THINGS TO TRY

Try the extreme settings. 

## EXTENDING THE MODEL

Make extraverts also experience something from social battery. 

## RELATED MODELS

The Playground Model from Hofstede, Student and Kramer partly served as a stepping stone regarding the status and meeting of agents. 

## CREDITS AND REFERENCES

The Friendship Field - Dyadic Friendship Network Formation based on Extraversion, Status and Resemblance driven by Social Battery (report by van de Kieft, C. & Timmer, E.)  
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
  <experiment name="experiment" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks &gt; 1097</exitCondition>
    <metric>mean [ friends-counter ] of turtles with [ status &gt; 0.5 ]</metric>
    <metric>standard-deviation [ friends-counter ] of turtles with [ status &gt; 0.5 ]</metric>
    <metric>mean [ friends-counter ] of turtles with [ status &lt; 0.5 ]</metric>
    <metric>standard-deviation [ friends-counter ] of turtles with [ status &lt; 0.5 ]</metric>
    <enumeratedValueSet variable="introversion-weight">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sb-threshold">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adaptive-characteristics?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-humans">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-characteristics">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="status-weight">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="personality-weight">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="interact-battery-constant">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="field-battery-constant">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="friendship-threshold">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="relation-weight">
      <value value="0.9"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="personalityOFF" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1097"/>
    <metric>[ appeal ] of turtles</metric>
    <metric>[ kindness ] of turtles</metric>
    <metric>[ friends-counter ] of turtles</metric>
    <enumeratedValueSet variable="introversion-weight">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sb-threshold">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adaptive-characteristics?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-humans">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-characteristics">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="status-weight">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="personality-weight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="interact-battery-constant">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="friendship-threshold">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="field-battery-constant">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="relation-weight">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="number_characteristics" repetitions="5" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1097"/>
    <metric>mean [ friends-counter ] of turtles</metric>
    <enumeratedValueSet variable="introversion-weight">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sb-threshold">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adaptive-characteristics?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-humans">
      <value value="70"/>
    </enumeratedValueSet>
    <steppedValueSet variable="number-of-characteristics" first="1" step="2" last="20"/>
    <enumeratedValueSet variable="status-weight">
      <value value="0.33"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="personality-weight">
      <value value="0.33"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="interact-battery-constant">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="friendship-threshold">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="field-battery-constant">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="relation-weight">
      <value value="0.33"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="adaptive-chars-links" repetitions="5" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1097"/>
    <metric>[ resemblance ] of links</metric>
    <metric>[ relation ] of links</metric>
    <enumeratedValueSet variable="sb-threshold">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="introversion-weight">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adaptive-characteristics?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-humans">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-characteristics">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="status-weight">
      <value value="0.33"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="personality-weight">
      <value value="0.33"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="interact-battery-constant">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="friendship-threshold">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="field-battery-constant">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="relation-weight">
      <value value="0.33"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="sb-threshold">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="introversion-weight">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adaptive-characteristics?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-humans">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-characteristics">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="status-weight">
      <value value="0.33"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="personality-weight">
      <value value="0.33"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="interact-battery-constant">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="friendship-threshold">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="field-battery-constant">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="relation-weight">
      <value value="0.33"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="extraversion_effect" repetitions="5" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1097"/>
    <metric>mean [ friends-counter ] of turtles with [ extraversion &gt; 0 ]</metric>
    <metric>mean [ friends-counter ] of turtles with [ extraversion &lt; 0 ]</metric>
    <enumeratedValueSet variable="sb-threshold">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="introversion-weight">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adaptive-characteristics?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-humans">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-characteristics">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="status-weight">
      <value value="0.33"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="personality-weight">
      <value value="0.33"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="interact-battery-constant">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="friendship-threshold">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="field-battery-constant">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="relation-weight">
      <value value="0.33"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="personalityON" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1097"/>
    <metric>[ appeal ] of turtles</metric>
    <metric>[ kindness ] of turtles</metric>
    <metric>[ friends-counter ] of turtles</metric>
    <enumeratedValueSet variable="introversion-weight">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sb-threshold">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adaptive-characteristics?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-humans">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-characteristics">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="status-weight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="personality-weight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="interact-battery-constant">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="friendship-threshold">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="field-battery-constant">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="relation-weight">
      <value value="0"/>
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
