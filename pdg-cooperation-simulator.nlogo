extensions [ nw ]

;;;;; VARIABLES ;;;;;

globals [
  world-size       ; world size set by slider (this has to be same during the simulation)

  total-company-value      ; total number of money in the company
  last-total-company-value ; total number of money in the company last evaluation
  hiring-decision-last-total-company-value

  initial-budget   ; initial budget divided for salaries each tick; this is constant number used
                   ; for applying boundaries for flexible budget variable
  budget           ; budget divided for salaries each tick; the budget is a part of total-company-value
                   ; this is restricted by the minimal and maximal limit of initial-budget
  budget-new       ; budget that will be used next period of time. This is needed due to wage
                   ; increase/decrease recalculation.

  profit           ; profit of the company counted each evaluation
  init-value       ; initial value

  count-fired-sick        ; count the number of employees that were fired because of their sickness
  count-fired-performance ; count the number of employees that were fired because of their low performance
  count-strategies        ; count the number of employees that have this strategy

  ;;network related
  clustering-coefficient               ; the clustering coefficient of the network; this is the
                                       ; average of clustering coefficients of all turtles
  average-path-length                  ; average path length of the network
  clustering-coefficient-of-lattice    ; the clustering coefficient of the initial lattice
  average-path-length-of-lattice       ; average path length of the initial lattice

  mean-degree

  infinity                             ; a very large number.
                                       ; used to denote distance between two turtles which
                                       ; don't have a connected or unconnected path between them
  number-rewired                       ; number of edges that have been rewired. used for plots.
  rewire-one?                          ; these two variables record which button was last pushed
  rewire-all?

  sum-of-performance-new  ; counts the sum of performances of all turtles (it is global because of code effectivity)

  tmp-fluct

  current-topology        ;remembering the current topology to choose method for linking new employee to an existing graph structure
]

turtles-own [
  turtle-id
  strategy          ; PD strategy
  diligence         ; specifies how much is agent's effort affected by salary changes
  defect-now?       ; true if the employee is going to defect during PD game
  has-partner?      ; specifies if the employee has a partner for cooperation
  partner           ; partner (another employee) for this round
  partner-id        ; coordinates of the partner in partner-history list
  partner-defected? ; true if the partner defects within this round
  partner-history   ; a list containing information about past interactions with other employees
  partner-history-longer   ; a list containing information about past interactions with other employees two
                           ; ticks back, this is because of tit for two tats strategy

  initial-value     ; the amount of money that employee gets in this tick
  generated-value   ; the amount of money produced by the employee in this tick
  sum-generated-value ; generated value for one period given by boss-reaction-time slider
  own-performance   ; current performance of the employee - what part of their own-value he/she gets (in percent)
  own-performance-old     ; used to store original value when own-performance is recalculated in one tick
  low-performance-counter ; counts the number of periods that employee is on lower-performance-limit
  productivity      ; specifies the ability of the employee to generate money
  self-evaluation   ; it specifies the effectivity of the employee in one period given by boss-reaction-time slider
  wage-change       ; this represents the change of employee's wage. The wage can be increased eventhough the
                    ; self-evaluation is negative (if the budget is increased).
  wage-change-tmp   ; stores information when wage-change is normalized
  own-degree
  is-hub?
  generated-value-balancing-PD ; value that was lost (or gained) by defecting during PD interaction

  stress-actual     ; current stress level of the employee, initially set to 0
  stress-limit      ; limit of stress bearable by the employee

  effort            ; the inner effort of the employee (0 means he/she does not work at all, 100 is maximum)
  old-wage-to-work-coef ; the amount of money received compared to amount of work done given in previous month

  is-sick?          ; specifies if the employee is sick and thefore he/she does not produce any value.
                    ; Note: Sick employee does not produce any value. However, the initial-value is added
                    ; to the sum-generated-value (because he/she should not be punished because of sickness)
  sick-timer        ; specify the time employee will be still sick
  sick-counter      ; specify the number of sicknesses

  number-of-defects ; number of the defects for last period of time given by boss-reaction-time slider
  number-of-cooperation ; number of my cooperations for last period of time given by boss-reaction-time slider
  sum-of-all-incomes ; sum that represents the total money generated by the employee

  node-clustering-coefficient
  distance-from-other-turtles ; list of distances of this node from other turtles

  fitness ; coefficient for Bianconi Barabasi algorthm
  group
]

links-own [
  rewired?                    ; keeps track of whether the link has been rewired or not
]

;;;;; CONSTANTS ;;;;;

to-report initial-effort
  report 75 ; represents initial value of effort for each employee
end

; pay-off matrix
to-report both-cooperate
  report 1.25 ; both cooperate
end

to-report coop-defect
  report 0 ; employee cooperates, partner defects
end

to-report defect-coop
  report 2.25 ; employee defects, partner cooperates
end

to-report both-defect
  report 1 ; both defect
end

to-report stress-multiplier-treason
  report 2 ; multiplies the stress added in case the employee cooperates and partner defects
end

to-report effort-boarder
  report 90 ; specifies the boarder, where decreasing wage is stimulating my effort (less than
            ; this number) and decreasing it (more than this number)
end

to-report world-size-const
  report 15 ; specifies the size of world
end

to-report stress-container
  report mean-value-stress-limit ; specifies maximal possible change of stress
end

to-report max-number-of-employees
  report 400
end

;;;;;; SETUP ;;;;;

to setup
  set-default-shape turtles "circle"
  reset-ticks
  set-patch-size 20

  ; following variables are set to the sum of initial wages of employees
  set total-company-value init-value
  set initial-budget init-value
  set last-total-company-value init-value
  set hiring-decision-last-total-company-value init-value

  ; following global variable checks number of fired employees based on their strategy
  set count-fired-sick [ 0 0 0 0 0 0 0 ]
  set count-fired-performance [ 0 0 0 0 0 0 0 ]
  set count-strategies [ 0 0 0 0 0 0 0 ]
end


to setup-count-all-strategies
  set count-strategies replace-item 0 count-strategies (count turtles with [ strategy = "defect" ])
  set count-strategies replace-item 1 count-strategies (count turtles with [ strategy = "cooperate" ])
  set count-strategies replace-item 2 count-strategies (count turtles with [ strategy = "tit-for-tat-npm" ])
  set count-strategies replace-item 3 count-strategies (count turtles with [ strategy = "tit-for-tat" ])
  set count-strategies replace-item 4 count-strategies (count turtles with [ strategy = "tit-for-two-tats" ])
  set count-strategies replace-item 5 count-strategies (count turtles with [ strategy = "pavlov" ])
  set count-strategies replace-item 6 count-strategies (count turtles with [ strategy = "unforgiving" ])
end


; Sets all basic values for each employee
to setup-set-basic-values-for-employee
    set productivity random-normal mean-value-productivity std-deviation-productivity
    set stress-limit round random-normal mean-value-stress-limit std-deviation-stress-limit
    set group random number-of-communities
    set fitness random-float 1.0
    set is-sick? false
    set sick-counter 0
    set own-performance 100
    set low-performance-counter 0
    set number-of-defects 0
    set number-of-cooperation 0
    set stress-actual 0
    set plabel-color red
    set effort initial-effort
    set sum-of-all-incomes 0
    set is-hub? false

    stress-recolor-agent ; recolor the agent - based on his/her actual stress level
end


; Sets common attributes for each turtle
to make-node-turtle-attributes
  set turtle-id who
  set color white
  ; initialize variables for each turtle
  setup-generate-strategy  ; set the strategy of the employee
  setup-generate-diligence   ; set the diligence of the employee
  setup-set-strategy-label ; prints the label for each employee based on their strategy

  ; set all basic values for each employee
  set productivity random-normal mean-value-productivity std-deviation-productivity ; set productivity mean-value
  set stress-limit round random-normal mean-value-stress-limit std-deviation-stress-limit
  set group random number-of-communities
  set fitness random-float 1.0
  set is-sick? false
  set sick-counter 0
  set own-performance 100
  set low-performance-counter 0
  set number-of-defects 0
  set number-of-cooperation 0
  set label-color red
  set effort initial-effort
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; STOCHASTIC BLOCK MODEL ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to setup-stochastic-block
  clear-all
  set world-size num-nodes
  resize-world -1 * world-size-const world-size-const -1 * world-size-const world-size-const
  set init-value initial-wage * world-size
  setup
  setup-nodes-stochastic-block
  setup-count-all-strategies
  setup-history-lists     ; set all lists that are needed for remembering history of cooperation between employees

  repeat 3 [ layout-spring (turtles with [any? link-neighbors]) links 0.4 6 1 ] ;; lays the nodes in a triangle
  set budget initial-wage * count turtles
end

to setup-nodes-stochastic-block
   create-turtles num-nodes [
    make-node-turtle-attributes
    set color (group * 10) + 6
    setxy random-xcor random-ycor
  ]

  create-edges-stochastic-block
end


to create-edges-stochastic-block
  ask links [ die ]
  ask turtles [
    ask turtles with [ who > [ who ] of myself ] [
      if random-float 1.0 < connection-probability group [group] of myself [
        create-link-with myself
      ]
    ]
  ]
end


to-report connection-probability [other-group my-group]
  ifelse (other-group = my-group)
    [ report wiring-probability-inside-community ]
    [ report wiring-probability-outside-community ]
end



;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; WATTS STROGATZ ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;


to setup-watts-strogatz
  clear-all
  set world-size num-nodes
  resize-world -1 * world-size-const world-size-const -1 * world-size-const world-size-const
  set init-value initial-wage * world-size
  setup
  setup-nodes-watts-strogatz
  setup-count-all-strategies

  ;; setting the values for the initial lattice
  set clustering-coefficient-of-lattice clustering-coefficient
  set average-path-length-of-lattice average-path-length
  set number-rewired 0

  setup-history-lists     ;; set all lists that are needed for remembering history of cooperation
                          ;; between employees
  rewire-all
  set budget initial-wage * count turtles
end


to setup-nodes-watts-strogatz
  repeat num-nodes [
    make-node-w-s
  ]
  layout-circle (sort turtles) max-pxcor - 1
end


to make-node-w-s
  create-turtles 1 [
    make-node-turtle-attributes
    setxy random-xcor random-ycor
  ]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; BARABASI ALBERT ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup-barabasi-albert
  clear-all
  set world-size num-nodes
  resize-world -1 * world-size-const world-size-const -1 * world-size-const world-size-const
  set init-value initial-wage * world-size
  setup
  setup-nodes-barabasi-albert
  setup-count-all-strategies
  setup-history-lists     ; set all lists that are needed for remembering history of cooperation between employees

  do-calculations-b-a
  set budget initial-wage * count turtles
  count-degree
end


to setup-nodes-barabasi-albert
  make-node-b-a nobody        ; first node, unattached
  make-node-b-a turtle 0
    repeat world-size - 2 [     ; 2 turtles are already created above
    make-node-b-a find-partner ; find partner & use it as attachment
    layout
  ]
end


to make-node-b-a [old-node]
  create-turtles 1 [
    make-node-turtle-attributes
    if old-node != nobody
      [ create-link-with old-node ;;[ set color green ]
        ; position the new node near its partner
        move-to old-node
        fd 8
      ]
  ]
end


; This code is the heart of the "preferential attachment" mechanism, and acts like
; a lottery where each node gets a ticket for every connection it already has.
; While the basic idea is the same as in the Lottery Example (in the Code Examples
; section of the Models Library), things are made simpler here by the fact that we
; can just use the links as if they were the "tickets": we first pick a random link,
; and than we pick one of the two ends of that link.
to-report find-partner
  report [one-of both-ends] of one-of links
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; BIANCONI BARABASI ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to setup-bianconi-barabasi
  clear-all
  set world-size num-nodes
  resize-world -1 * world-size-const world-size-const -1 * world-size-const world-size-const
  set init-value initial-wage * world-size
  setup
  setup-nodes-bianconi-barabasi
  setup-count-all-strategies
  setup-history-lists     ; set all lists that are needed for remembering history of cooperation between employees

  set budget initial-wage * count turtles
  count-degree

end


to setup-nodes-bianconi-barabasi
  make-node-b-b nobody        ; first node, unattached
  make-node-b-b turtle 0
    repeat world-size - 2 [     ; 2 turtles are already created above
    make-node-b-b find-partner-b-b ; find partner & use it as attachment
    layout
  ]
end

to make-node-b-b [old-node]
  create-turtles 1 [
    make-node-turtle-attributes
    if old-node != nobody
      [ create-link-with old-node ;;[ set color green ]
        ; position the new node near its partner
        move-to old-node
        fd 8
      ]
  ]
end


to-report find-partner-b-b
  let total random-float sum [(count link-neighbors) * fitness] of turtles
  let node-partner nobody
  ask turtles
  [ let nc (count link-neighbors) * fitness
    ;; if there's no winner yet...
    if node-partner = nobody
    [ ifelse nc > total
        [ set node-partner self ]
        [ set total total - nc ] ] ]
  report node-partner
end


to community-detection
  nw:set-context turtles links
  color-clusters nw:louvain-communities
end


to color-clusters [ clusters ]
  ; reset all colors
  ask turtles [ set color gray ]
  ask links [ set color gray - 2 ]
  let n length clusters
  ; Generate a unique hue for each cluster
  let hues n-values n [ i -> (360 * i / n) ]

  ; loop through the clusters and colors zipped together
  (foreach clusters hues [ [cluster hue] ->
    ask cluster [ ; for each node in the cluster
                  ; give the node the color of its cluster
      set color hsb hue 100 100
      ; Color links contained in the cluster slightly darker than the cluster color
      ask my-links with [ member? other-end cluster ] [ set color hsb hue 100 75 ]
    ]
  ])
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; ZACHARY'S KARATE CLUB ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to zachary-karate-club

  clear-all

  ; https://github.com/softrebel/zachary-visualize/blob/main/zachary.graphml
  nw:load-graphml "zachary.graphml"
  set world-size 36
  resize-world -1 * world-size-const world-size-const -1 * world-size-const world-size-const
  set init-value initial-wage * world-size
  setup

  ask turtles [
    make-node-turtle-attributes
  ]

  setup-count-all-strategies
  setup-history-lists     ; set all lists that are needed for remembering history of cooperation between employees

  repeat 3 [ layout-spring (turtles with [any? link-neighbors]) links 0.4 6 1 ] ;; lays the nodes in a triangle
  set budget initial-wage * count turtles

end


;;;;; RUN ;;;;;

to go
  ; PD game initialization and its storage in the history
  ask turtles [
    ; choose one partner from his/her healthy neighbors
    ifelse count link-neighbors with [ not is-sick? ] > 0 [
      set has-partner? true
      set partner one-of link-neighbors
    ]
    [ set has-partner? false ]

    ;; find the coordines of partner in history tables
    if has-partner? [ set partner-id [turtle-id] of partner]
    ;; this set the the defect-now? and partner-defected? variables
    ;; based on employee's and partner's strategy
    if has-partner? [ do-resolve-PD-action-this-round ]

    ; update-history-tables
    if has-partner? [
      if strategy = "tit-for-tat" [ tit-for-tat-history-update ]
      if strategy = "tit-for-tat-npm" [ tit-for-tat-history-update ]
      if strategy = "tit-for-two-tats" [ tit-for-two-tats-history-update ]
      if strategy = "unforgiving" [ unforgiving-history-update ]
    ]
  ]

  ; employee's money initialization
  let sum-own-performance sum [ own-performance ] of turtles
  ask turtles [
    ; set iniatial part of budget - it depends on the employee's performance
    set initial-value own-performance * budget / sum-own-performance
  ]

  ; stress update
  ask turtles [
    ; update stress-regen-coef based on the employees performance last period of time
    ; it depends on employee's effort - higher value of effort means less regeneration
    let stress-coef effort / boss-reaction-time
    ; stress is increased based on level of effort - impact can be adjusted by parameter
    if not is-sick? [ set stress-actual (stress-actual + stress-container * stress-coef * effort-stress-increase) ]
    ; stress regeneration - it could not be lower than 0
    set stress-actual max list 0 (stress-actual - (stress-container * stress-regen))
  ]

  let new-sum-value 0  ; counts the sum of money generated this ticks

  ; money generation (effort is involved)
  ask turtles [
    ifelse not is-sick?
    [
      ; counts the money generated this tick by employee in PD
      if has-partner? [ generate-money-based-on-PD ]

      ; counts the money generated by employee's own work
      let basic-part initial-value * (1 - cooperation-part)
      set generated-value generated-value + (basic-part * productivity * (effort * 0.01))
    ]
    [
      set generated-value 0  ; emploeyee is not working this tick
      ; W: Not punishing employee for being sick. It adds his/her initial value to the sum
      set sum-generated-value sum-generated-value + initial-value
    ]
  ]

  ; final recalculations of generated money in the company
  ask turtles [
    ; company pays the wage to employee
    set generated-value generated-value - initial-value
    ; update sum based on the generated values
    set new-sum-value new-sum-value + generated-value
    set sum-generated-value sum-generated-value + generated-value
    set sum-of-all-incomes sum-of-all-incomes + generated-value

    ; reseting generated value for next round
    set generated-value 0
  ]

  ; W: update stress level according to PD and count the number of cooperations and defects
  ; No stress is added when both cooperates
  ; Stress is added when both defects
  ; Stress multiplied by stress-multiplier-treason is added to the employee (if he/she cooperates and partner defects)
  ; Stress multiplied by stress-multiplier-treason is added to the partner (if the employee defects and partner cooperates)

  ; This also counts number of cooperation and defects
  ; This also counts generated-value-balancing-PD - if employee cooperates and partner defects, lost generated value
  ; is stored here for employee and same is value is taken away from parner's generated-value-balancing-PD
  ask turtles [

    if not is-sick? and has-partner? [

      ; both cooperate - they do not get stress at all
      if not defect-now? and not partner-defected? [
        set number-of-cooperation number-of-cooperation + 1
        ask partner [
          set number-of-cooperation number-of-cooperation + 1
        ]
      ]

      ; both defected
      if defect-now? and partner-defected? [
        set stress-actual stress-actual + stress-container * stress-modification-on-PD * cooperation-part
        set number-of-defects number-of-defects + 1
        ask partner [
          set stress-actual stress-actual + stress-container * stress-modification-on-PD * cooperation-part
          set number-of-defects number-of-defects + 1
        ]
      ]

      ; this specify how many value was lost for employee that cooperates and his/her partner defects
      let stolen-generated-value 0
      if boss-insight-cooperation-probability >= random-float 1 [
        set stolen-generated-value initial-value * cooperation-part * both-cooperate * boss-insight-cooperation-part
      ]
      ; employee cooperates, partner defects
      if not defect-now? and partner-defected? [
        set stress-actual stress-actual + stress-container * stress-modification-on-PD * cooperation-part * stress-multiplier-treason
        set number-of-cooperation number-of-cooperation + 1
        set generated-value-balancing-PD generated-value-balancing-PD + stolen-generated-value
          ask partner [
            set number-of-defects number-of-defects + 1
            set generated-value-balancing-PD generated-value-balancing-PD - stolen-generated-value
          ]
      ]

      ; in this case the stolen value is counted from the partners initial value
      if boss-insight-cooperation-probability >= random-float 1 [
        ask partner [
          set stolen-generated-value initial-value * cooperation-part * both-cooperate * boss-insight-cooperation-part
        ]
      ]
      ; employee defects, partner cooperates
      if defect-now? and not partner-defected? [
        set number-of-defects number-of-defects + 1
        set generated-value-balancing-PD generated-value-balancing-PD - stolen-generated-value
        ask partner [
          set stress-actual stress-actual + stress-container * stress-modification-on-PD * cooperation-part * stress-multiplier-treason
          set number-of-cooperation number-of-cooperation + 1
          set generated-value-balancing-PD generated-value-balancing-PD + stolen-generated-value
        ]
      ]
    ]

    check-stress-level     ; check if the employee is not sick and border values
    stress-recolor-agent  ; recoloring agents based on their stress-actual level
  ]

   ; update sick timer or make employee healthy
   ask turtles [
     if is-sick? [
       set sick-timer sick-timer - 1
       if sick-timer < 1 [
         ; this makes employee healthy
         ; currently it also changes the employee's own-performance and effort
         make-employee-healthy
       ]
     ]
   ]

  ; update global value
  set total-company-value total-company-value - budget + new-sum-value

  ; budget update (this can be turned off by setting budget change to 0)
  if (ticks mod boss-reaction-time) = 0 and ticks != 0 [
    set profit total-company-value - last-total-company-value ; profit is the difference between old and new value
    set last-total-company-value total-company-value ; reset last total value to new one
    set budget-new budget + profit * budget-change ; recount budget
    set budget-new budget-new + initial-budget * fixed-budget-change  ; fixed budget change added
    set budget-new min list budget-new (initial-budget * maximal-budget) ; cut it from top
    set budget-new max list budget-new (initial-budget * minimal-budget) ; cut it from bottom
  ]

  ; this recalculates the own-performance based on number of cooperation and generated money
  ; the recaluculation involves boss insight, boss-reaction-intensity and sum-generated-vs-cooperation
  if (ticks mod boss-reaction-time) = 0 and ticks != 0
  [
    ; firstly, we compensate everything lost in the PD cooperation
    ask turtles [
      set sum-generated-value sum-generated-value + generated-value-balancing-PD
    ]

    ; specify the maximum of values that were produced by the employees during one period of time
    let max-new-values max [sum-generated-value - boss-reaction-time * initial-value] of turtles
    let min-new-values min [sum-generated-value - boss-reaction-time * initial-value] of turtles

    ; count total number of cooperation divided by total number of interactions
    let sum-of-cooperations sum [ number-of-cooperation ] of turtles
    let sum-of-interactions sum-of-cooperations + sum [ number-of-defects ] of turtles

    ; recalculate the average of generated values
    ask turtles [
      ; normalize to value 0 to 1
      let tmp-sum-coef 0
      if max-new-values - min-new-values != 0 [
        ; TODO: this should be revised if this is a correct approach (more approaches can be applied)
        ; the problem could arise when values are close to each other
        set tmp-sum-coef ((sum-generated-value - boss-reaction-time * initial-value) - min-new-values) / (max-new-values - min-new-values)
      ]

      ; use boss insight for performance and count self evaluation
      set self-evaluation boss-insight-performance * tmp-sum-coef + (1 - boss-insight-performance) * random-float 1
    ]

    let average-self-evaluation sum [self-evaluation] of turtles / count turtles ; variable used for evaluation of employee
    let sum-of-performance-old sum [ own-performance ] of turtles ; used for wage change calculation
    if average-self-evaluation > 0 [
      ask turtles [
        ; change against average multiplicated by reaction
        ; result should be below zero if the agent evaluation is below average, zero if on average and above 0 when better than average
        set self-evaluation (self-evaluation / average-self-evaluation) - 1
        set own-performance-old own-performance ; remember the previous own-performance
        set own-performance own-performance + own-performance * self-evaluation * boss-reaction-intensity ; set it to the new value based on self-evaluation
        let old-diff own-performance - own-performance-old ; diff between old own-performance and new one
        if own-performance > performance-upper-limit [ set own-performance performance-upper-limit ]
        if own-performance < performance-lower-limit [ set own-performance performance-lower-limit ]
      ]
    ]


    set sum-of-performance-new sum [ own-performance ] of turtles
    let sum-of-performance-new-increased sum [ own-performance ] of turtles with [ own-performance >= own-performance-old ]

    ;;; WAGE DISTRIBUTION STRATEGIES CHOOSER ;;;
    ; we may decide to apply different strategies on how to distribute money to employees


    if wage-distribution-strategy = "everybody" [
    ; this option distibutes profit to everyone even if their own-performance is decreased (but the actual value differs, i.e. no communism)
    ; however, if we decide to turn on budget growth (budget-change or fixed-budget-change sliders) then
    ; the employees might get more money even if they have worsened their performance
    ; (technically: employees' wage-change can be positive even though their own-performance is lowered)

      ask turtles [
        ; wage change depends on self-evaluation but also on budget change. Therefore, if budget grows, then wage can be higher
        ; even though new own-performance is lower than old one
        ; it is calculated as agent's money per performance at the end of a tick minus the same at the beggining of the tick = clear added value per point of performance in given tick
        set wage-change (budget-new * own-performance / sum-of-performance-new) - (budget * own-performance-old / sum-of-performance-old)
      ]
    ]


    if wage-distribution-strategy = "increasing-own-perf" [
      ; this option distibutes profit only to those employees that increased their performance
      ; those who got worse will only get the same amount as in previous month
      ; the extra money that were produced this month will be proportionally divided between the employees that are doing better than in previous month

      ask turtles [
        let budget-change-local budget-new - budget
        ; dealing with situation when company is not profitting
        ifelse budget-change-local <= 0 [
          ; distributing lower budget. Those with lower performance will definitely have less money
          ; those with higher performance may have more or less money, depending on the actual budget decrease
          set wage-change (budget-new * own-performance / sum-of-performance-new) - (budget * own-performance-old / sum-of-performance-old)
        ]
        [
        ; in this part of code, every employee will get the same portion of last month budget, i.e. the base salary is same for everyone
          set wage-change (budget * own-performance / sum-of-performance-new) - (budget * own-performance-old / sum-of-performance-old)
          if own-performance >= own-performance-old [
            ; and the following line distributes the extra money among those emplyoees what got better
            set wage-change wage-change + budget-change-local * own-performance / sum-of-performance-new-increased
          ]
        ]
      ]
    ]


    ask turtles [
      set wage-change-tmp 0
     ; normalization of wage against last month
      if (budget * own-performance-old / sum-of-performance-old) > 0 [
        set wage-change-tmp wage-change / (budget * own-performance-old / sum-of-performance-old)
      ]
    ]
  ]



  ; this recalculates the stress level based on the wage increase or decrease
  ; the recaluculation involves evaluation-stress-change
  ; this is done only when evaluation stress increse is allowed
  ; W: when the wage (own-performance) is lowered, then the stress level is increased:
  ; - if it on minimal value, then the coeficient is multiplied by two
  ; - if it is 100, then the coeficient is 1
  ; - if it on maximal value, then the coeficient is divided by two
  ; when the wage (own-performance) is increased, then the stress level is decreased (coeficient is set to 1)
  if (ticks mod boss-reaction-time) = 0 and ticks != 0 [
    ask turtles [
      ; count coef - the effect of wage change is bigger for employess with lower salary (easterlinn paradox)
      ; in other words, coef-dist defines how sensitive is an employee towards wage change
      let coef-dist 1
        ifelse own-performance <= 100
        [
          let real-dist 100 - own-performance
          let whole-dist 100 - performance-lower-limit
          set coef-dist 1 + (real-dist / whole-dist)
        ]
        [
          let real-dist performance-upper-limit - own-performance
          let whole-dist performance-upper-limit - 100
          set coef-dist 0.5 + (real-dist / (2 * whole-dist))
        ]

      ; update stress based on income
      ; wage-change-tmp can be positive or negative. Therefore stress is increased when wage is decreased
      ; and stress is decreased when wage is increased.
      set stress-actual stress-actual - coef-dist * wage-change-tmp * evaluation-stress-change

    ]
  ]

  ; this recalculates the effort based on the wage increase or decrease
  if (ticks mod boss-reaction-time) = 0 and ticks != 0 [
    ask turtles [
      ; total amount of work done by an employee:
      ; own - part of work that has to be done by the employee
      ; requested from others = how many times employee agreed to cooperate with other employees. The scope of exchanged work depends on
      ;  cooperation part slider
      let workload (own-performance * (1 - cooperation-part) * boss-reaction-time) + cooperation-part * own-performance * number-of-cooperation
      ; amount of money received compared to amount of work done
      ; in the numerator there is wage of the employee
      ; in the denominator there is amount of work done by an employee
      ; therefore, this coeficient represents how "hard" it was for the employee to get money based on the amount of work done
      ; the higher the coeficient, the better for an employee (because he gets more money for the same amount of work or same money for less work)
      let wage-to-work-coef 0
      if workload != 0 [
        set wage-to-work-coef (budget-new * own-performance / sum-of-performance-new) / workload
      ]

      ; above zero values of coef-diff mean that an employee has increased their money-to-work-done ratio (good for him)
      ; the value results from comparison between this and last month (or other period of time, as defined by boss-reaction-time)
      ; we only differetiate the positive and negative value, the particular value does not influence how much the effort is changed
      ; at the end of the first month, it will be always positive
      let coef-diff wage-to-work-coef - old-wage-to-work-coef
      ; diff specifies, how much the effort should be changed and it is based on wage change and max-effort-change slider
      ; max-effort-change = how big part of effort is changed (0.1 means 10% of initial effort) by wage change effects
      ; diff takes absolute values such that the table below adjusts the effort levels correctly
      let diff abs wage-change-tmp * initial-effort * max-effort-change

      ; this part describes a matrix of 4 possible situations when an employee evaluates their money-to-work-done ratio and the overall salary
      ; please note that coef-diff can be positive even though the salary was decreased, i.e., the overall salary is lower, but the agent
      ; has worked less, also. From the agent's point of view, the money to work done comparison between this and previous month is better.

      ; DILIGENCE is taken into account:
      ; diligence has normal distribution across population of agents with mean value 0.5 and std 0.175 --> <0,1>
      ; (defined in the code, not by a slider)
      ; diligence defines how much will the effort be affected by salary changes.
      ; particular effects are specified in the matrix below


      if ticks > boss-reaction-time [
        ; in case that salary was decreased
        ifelse wage-change < 0 [
          ifelse coef-diff > 0 [
            ; but it was easier for employee to get money
            let effort-diff diff * diligence
            ; the effort will be increased
            set effort effort + effort-diff
          ]
          [
            ; or it was harder for employee to get money
            let effort-diff diff * (1 - diligence)
            ; the effort will be decreased
            set effort effort - effort-diff
          ]
        ]
        [
          ; in case that salary was increased
          ifelse coef-diff > 0 [
            ; and it was easier for employee to get money
            let effort-diff diff * (1 - diligence)
            ; the effort will be decreased
            set effort effort - effort-diff
          ]
          [
            ; but it was harder for employee to get money
            let effort-diff diff * diligence
            ; then the effort will be increased
            set effort effort + effort-diff
          ]
        ]
      ]
      ; stores coef for comparison in next month
      set old-wage-to-work-coef wage-to-work-coef
    ]
  ]

  ; check all values
  if (ticks mod boss-reaction-time) = 0 and ticks != 0 [
    ask turtles [
      check-stress-level
      stress-recolor-agent
      check-effort

      ifelse own-performance = performance-lower-limit
        ; if the employee's performance is the lowest possible, increase the counter
        [ set low-performance-counter low-performance-counter + 1 ]
        ; if the employee's performance is not the lowest possible, decrease the counter
        [ set low-performance-counter max list 0  (low-performance-counter - 1) ]
    ]
  ]

  ; fire an employee who is sick too much or not performing well
  if (ticks mod boss-reaction-time) = 0 and ticks != 0 [
    ask turtles [
      let index strategy-index strategy ; gets the strategy of the employee

      if (sick-counter >= patience) [
        let tmp-item item index count-fired-sick
        set count-fired-sick (replace-item index count-fired-sick (tmp-item + 1))
      ]
      if (low-performance-counter >= patience) [
        let tmp-item item index count-fired-performance
        set count-fired-performance (replace-item index count-fired-performance (tmp-item + 1))
      ]

      ; replace the old employee with new one
      if (sick-counter >= patience or low-performance-counter >= patience) [

        ifelse (random-float 1.0 < 0.8)
          [leave-company]
          [replace-employee]
      ]
    ]
  ]


  ; reset values after evaluation
  if (ticks mod boss-reaction-time) = 0 and ticks != 0 [
    ask turtles [
      set sum-generated-value 0 ; resets the value
      set number-of-defects 0  ; reset defects
      set number-of-cooperation 0  ; reset defects
    ]
  ]

  ; weekend - regenerate stress without influence of stress coef
  if ticks mod 5 = 0 and ticks != 0
  [
    ask turtles [
      ; regenerate stress level twice, two days in weekend
      set stress-actual max list 0 (stress-actual - (stress-container * stress-regen * 2))

      if is-sick? [
        set sick-timer sick-timer - 2
        if sick-timer < 1 [ ; check if employee recovered from sickness over weekend
          make-employee-healthy
        ]
      ]

      check-stress-level
      stress-recolor-agent
    ]
  ]

  ; set new budget as official one
  if (ticks mod boss-reaction-time) = 0 and ticks != 0 [
    set budget budget-new
  ]

  ; hire new emplyess -  0 - (5% of number of emplyees) per quarter, after certain company growth
  if (ticks mod 60 = 0 and hiring-decision-last-total-company-value * 1.2 < total-company-value and ticks != 0)[
    repeat (count turtles * 0.05) [
      hire-new-employee
    ]
    set hiring-decision-last-total-company-value total-company-value
  ]

  ; fire emplyees based on company performance
  if (ticks mod 60 = 0 and hiring-decision-last-total-company-value > total-company-value and ticks != 0)[
    repeat (count turtles * 0.05) [
      ask one-of turtles [
        ifelse (random-float 1.0 < 0.8)
          [ leave-company ]
          [ replace-employee ]
      ]
    ]
    set hiring-decision-last-total-company-value total-company-value
  ]

  ; natural emplyee fluctuation
  if (ticks mod 60 = 0 and ticks != 0)[
    repeat (count turtles * attrition-percentage / 4) [
      ask one-of turtles [
        ifelse (random-float 1.0 < 0.8)
          [ leave-company ]
          [ replace-employee ]
      ]
    ]
  ]


  tick
end


to hire-new-employee

  let length-of-partner-history length [partner-history] of one-of turtles

  if hiring-strategy = "random" [
    create-turtles 1 [
      make-node-turtle-attributes
      ask other turtles [
        if random-float 1.0 < connection-probability group [group] of myself [
          create-link-with myself
        ]
      ]
    ]
  ]

  if hiring-strategy = "preferential" [
    make-node-b-b find-partner-b-b
  ]

  layout
  setup-count-all-strategies

  let default-history []
  repeat length-of-partner-history [set default-history (fput false default-history)]
  repeat length-of-partner-history [set default-history (fput false default-history)]

  ask max-one-of turtles [turtle-id] [
    set partner-history default-history
    set partner-history-longer default-history
    set partner-defected? false
  ]

  ask turtles [
    set partner-history (lput false partner-history)
    set partner-history-longer (lput false partner-history-longer)
  ]

  ;;add penalization

end


to leave-company
  let max-degree -1
  let highest-degree-neighbor nobody
  ask link-neighbors [
    let current-degree [own-degree] of self
    if current-degree > max-degree [
      set max-degree current-degree
      set highest-degree-neighbor self
    ]
  ]
  if (highest-degree-neighbor != nobody) [
    ask link-neighbors [
      if (self != highest-degree-neighbor)[
        create-link-with highest-degree-neighbor
      ]
    ]
  ]
  die
end


to replace-employee

  let partner-history-length length partner-history
  let partner-history-longer-length length partner-history-longer

  setup-generate-strategy    ; set the strategy of the employee
  setup-generate-diligence   ; set the diligence of the employee
  setup-set-strategy-label   ; prints the label for each employee based on their strategy

  ; count newly generated strategy
  let index strategy-index strategy
  let tmp-item item index count-strategies
  set count-strategies (replace-item index count-strategies (tmp-item + 1))   ;;TODO CHECK

  setup-set-basic-values-for-employee

  ; penalization for initial training of new employee
  set total-company-value total-company-value - (penalisation-for-fluctuation * budget * boss-reaction-time / count turtles)
  set tmp-fluct tmp-fluct + (penalisation-for-fluctuation * budget * boss-reaction-time / count turtles)

  ; reset own history lists
  set partner-history []
  repeat partner-history-length [ set partner-history (fput false partner-history) ]
  set partner-history-longer []
  repeat partner-history-longer-length [ set partner-history-longer (fput false partner-history-longer) ]

  ;; reset neighbors perception of me
  ask (turtle-set self link-neighbors) [                             ;; change history lists of neighboring turtles
    set partner-history-longer (replace-item turtle-id partner-history false)
    set partner-history (replace-item turtle-id partner-history false)
  ]
end


; This function resets history of one patch in all history lists of all patches
; @coords is the index of patch in history lists
to reset-history-lists [coords]
  ask turtles[
    set partner-history (replace-item coords partner-history false)
    set partner-history-longer (replace-item coords partner-history false)
  ]
end

; This function makes the employee healthy and updates the performance and effort the situation
; when employee is sick
to make-employee-healthy
  set is-sick? false
  set stress-actual 0
  set sick-timer 0
  set sick-counter sick-counter + 1
  ; W: update performance when employee is sick
  if own-performance > 100 [ set own-performance 100 + (own-performance - 100) / 2 ]
  ; W: set extreme effort to initial value
  if effort > initial-effort [ set effort initial-effort + (effort - initial-effort) / 2 ]
  stress-recolor-agent
end

; This function checks the limit of effort
to check-effort
  if effort < 0 [ set effort 0 ]
  if effort > 100 [ set effort 100 ]
end

;;;;; GENERATED MONEY ;;;;;

; Function counts the money generated this tick by employee. It is based on emplyee productivity
; and cooperation part.
; Coeficient is given by constants: both-defect, coop-defect, defect-coop and both-cooperate
to generate-money-based-on-PD
  set partner-defected? [ defect-now? ] of partner
  ifelse partner-defected?
  [
    ifelse defect-now?
    [ update-generated-money-based-on-PD both-defect both-defect ]
    [ update-generated-money-based-on-PD coop-defect defect-coop ]
  ]
  [
    ifelse defect-now?
    [ update-generated-money-based-on-PD defect-coop coop-defect ]
    [ update-generated-money-based-on-PD both-cooperate both-cooperate ]
  ]
end

; Add generated value to the empoyee that initiate the interaction and his/her partner. It is
; based on cooperation part of work which is multiplicated by coeficient from PD game.
; @my-coef is coeficient that multiplicates cooperation part of the intial-value that is added
    ; the empoyee that initiate the interaction
; @partner-coef is coeficient that multiplicates cooperation part of the intial-value that is
    ; added to the partner
to update-generated-money-based-on-PD [ my-coef partner-coef ]
  let coop-part initial-value * cooperation-part
  set generated-value generated-value + coop-part * my-coef
  ask partner [
    set generated-value generated-value + coop-part * partner-coef
  ]
end

;;;;; STRESS ;;;;;

; Function recolor agent - based on his/her actual stress level
to stress-recolor-agent
  ifelse not is-sick?
  [
    set color black
    if stress-actual < stress-limit * 0.99999 [ set color 2 ]
    if stress-actual < stress-limit * 0.8 [ set color 4 ]
    if stress-actual < stress-limit * 0.6 [ set color 6 ]
    if stress-actual < stress-limit * 0.4 [ set color 8 ]
    if stress-actual < stress-limit * 0.2 [ set color white ]
  ]
te  [ set color yellow ]
end

; This function check the stress level and if the employee has the actual-stress at stress-limit
; than it makes them sick for few ticks given by slider: sick-slider
to check-stress-level
  if stress-actual < 0 [ set stress-actual 0 ]

  ; become sick only when random number is lower then sickness probability
  let rand-sickness random-float 1.0
  if stress-actual >= stress-limit and sickness-probability >= rand-sickness and not is-sick? [
      set is-sick? true
      set sick-timer sick-slider
  ]
end

;;;;; STRATEGIES ;;;;;

; Function sets the diligence of the employee. It is based on intial-diligence slider that specifies
; the portion of employees with defect strategy. Then the rest is equally divided between
; cooperative strategies - cooperative, tit-for-tat and unforgiving
to setup-generate-diligence
  set diligence random-normal 0.5 0.175
  if diligence > 1 [ set diligence 1 ]
  if diligence < 0 [ set diligence 0 ]
end


; Function set the cooperative strategy of the employee based on the initial sliders
to setup-generate-strategy
  let rand-coop random-float 1.0
  ifelse rand-coop < defect-strategy
    [ set strategy "defect" ]
    [ ifelse rand-coop < (defect-strategy + cooperate-strategy)
      [ set strategy "cooperate" ]
      [ ifelse rand-coop < (defect-strategy + cooperate-strategy + tit-for-tat-npm-strategy)
        [ set strategy "tit-for-tat-npm" ]
        [ ifelse rand-coop < (defect-strategy + cooperate-strategy + tit-for-tat-npm-strategy + tit-for-tat-strategy)
          [ set strategy "tit-for-tat" ]
          [ ifelse rand-coop < (defect-strategy + cooperate-strategy + tit-for-tat-npm-strategy + tit-for-tat-strategy + tit-for-two-tats-strategy)
            [ set strategy "tit-for-two-tats" ]
            [ ifelse rand-coop < (defect-strategy + cooperate-strategy + tit-for-tat-npm-strategy + tit-for-tat-strategy + tit-for-two-tats-strategy + pavlov-strategy)
              [ set strategy "pavlov" ]
              [ set strategy "unforgiving" ]
            ]
          ]
        ]
      ]
    ]
end


; Function returns index of strategy
to-report strategy-index [ strat ]
  if strat = "defect" [ report 0 ]
  if strat = "cooperate" [ report 1 ]
  if strat = "tit-for-tat-npm" [ report 2 ]
  if strat = "tit-for-tat" [ report 3 ]
  if strat = "tit-for-two-tats" [ report 4 ]
  if strat = "pavlov" [ report 5 ]
  if strat = "unforgiving" [ report 6 ]
end

; Function set all lists that are needed for remembering history of cooperation
; between employees
to setup-history-lists
  let num-turtles count turtles
  let default-history [] ; initialize the variable to be a list

  ; create a list with num-turtles elements for storing partner histories
  repeat num-turtles [ set default-history (fput false default-history) ]

  ; give each employee a copy of this list for tracking partner histories
  ask turtles [ set partner-history default-history ]
  ask turtles [ set partner-history-longer default-history ] ; setup second history array for tit for two tats strategy

  ask turtles [ set partner-defected? false ] ; prevent numbers from getting into history lists
end

; Function sets the label for each employee based on their strategy
to setup-set-strategy-label
  set label who
  if strategy = "cooperate" [ set label "C  " ]
  if strategy = "defect" [ set label "D  " ]
  if strategy = "tit-for-tat" [ set label "T  " ]
  if strategy = "tit-for-tat-npm" [ set label "nT  " ]
  if strategy = "unforgiving" [ set label "U  " ]
  if strategy = "pavlov" [ set label "P  " ]
  if strategy = "tit-for-two-tats" [ set label "T2  " ]
end

; This function selects action based on the employee's strategy
to do-resolve-PD-action-this-round
  if strategy = "cooperate" [ set defect-now? false ]
  if strategy = "defect" [ set defect-now? true ]
  if strategy = "tit-for-tat" [ tit-for-tat ]
  if strategy = "unforgiving" [ unforgiving ]
  if strategy = "tit-for-tat-npm" [ tit-for-tat-npm ]
  if strategy = "tit-for-two-tats" [ tit-for-two-tats ]
  if strategy = "pavlov" [ pavlov ]
end

; This function resolves the case if employee have Pavlov strategy
; if 3 or 5 my response is the same, if not I change my behaviour
to pavlov
  set partner-defected? [defect-now?] of partner

  if not is-boolean? partner-defected? [ set partner-defected? false ]
  if not is-boolean? defect-now? [ set defect-now? false ] ; check if defect-now? and
                                                           ; partner-defected? variables are already set

  ifelse partner-defected?
  [ set defect-now? not defect-now? ] ; partner defected
  [ set defect-now? defect-now? ]     ; partner cooperated
end

; This function resolves the case if employee have Tit For Two Tats strategy
to tit-for-two-tats
  let defection? item partner-id partner-history               ; get information from history list
  let defection-longer? item partner-id partner-history-longer ; get information from history list

  ifelse defection?
  [
    ifelse defection-longer?
      [ set defect-now? true ] ; both true
      [ set defect-now? false ]
  ]
  [ set defect-now? false ]
end

;; This function resolves the case if employee have Naive Peacemaker TFT strategy
to tit-for-tat-npm
  let defection? item partner-id partner-history ;; get information from history list

  ifelse (defection?)
  [
    let prob random-float 1.0
    ifelse prob <= tft-npm-peace-probability ; decide whether to make peace or not based
                                             ; on slider tft-npm-peace-probability
      [ set defect-now? false ]
      [ set defect-now? true ]
  ]
  [ set defect-now? false ]
end

; This function resolves the case if employee have tft strategy
to tit-for-tat
  let defection? item partner-id partner-history ; get information from history list

  ifelse (defection?)
  [ set defect-now? true ]
  [ set defect-now? false ]

end

; This function resolves the case if employee have unfrogiving strategy
to unforgiving
  let defection? item partner-id partner-history ; get information from history list

  ifelse (defection?)
  [ set defect-now? true ]
  [ set defect-now? false ]
end

; This function updates history table in tft strategy
to tit-for-tat-history-update
  set partner-history (replace-item partner-id partner-history partner-defected?)
end

; This function updates history tables in tf2t strategy
to tit-for-two-tats-history-update
  let defection? item partner-id partner-history                                  ; get information from history list
  set partner-history-longer (replace-item partner-id partner-history defection?) ; put that information to longer history list

  set partner-history (replace-item partner-id partner-history partner-defected?)
end

; This function updates history table in unforgiving strategy
to unforgiving-history-update
  if partner-defected?
  [ set partner-history (replace-item partner-id partner-history true) ]
end

;;;;; LAYOUT AND CALCULATIONS ;;;;;

; resize-nodes, change back and forth from size based on degree to a size of 1
to resize-nodes
  ifelse all? turtles [size <= 1]
  [
    ; a node is a circle with diameter determined by the SIZE variable; using SQRT makes the circle's
    ; area proportional to its degree
    ask turtles [ set size sqrt count link-neighbors ]
  ]
  [ ask turtles [ set size 1 ] ]
end

to layout
  ; the number 3 here is arbitrary; more repetitions slows down the
  ; model, but too few gives poor layouts
  repeat 5 [
    ; the more turtles we have to fit into the same amount of space,
    ; the smaller the inputs to layout-spring we'll need to use
    let factor sqrt count turtles
    ; numbers here are arbitrarily chosen for pleasing appearance
    layout-spring turtles links (1 / factor) (7 / factor) (1 / factor)
  ]
  ; don't bump the edges of the world
  let x-offset max [xcor] of turtles + min [xcor] of turtles
  let y-offset max [ycor] of turtles + min [ycor] of turtles
  ; big jumps look funny, so only adjust a little each time
  set x-offset limit-magnitude x-offset 0.1
  set y-offset limit-magnitude y-offset 0.1
  ask turtles [ setxy (xcor - x-offset / 2) (ycor - y-offset / 2) ]
end

to-report limit-magnitude [number limit]
  if number > limit [ report limit ]
  if number < (- limit) [ report (- limit) ]
  report number
end

; do-calculations-e-r reports true if the network is connected, and reports false if the network is disconnected.
; (In the disconnected case, the average path length does not make sense, or perhaps may be considered infinite)
to do-calculations-e-r
  ; find the path lengths in the network
  find-path-lengths

  let num-connected-pairs sum [length remove infinity (remove 0 distance-from-other-turtles)] of turtles

  ; In a connected network on N nodes, we should have N(N-1) measurements of distances between pairs,
  ; and none of those distances should be infinity.
  ; If there were any "infinity" length paths between nodes, then the network is disconnected.
  ; In that case, calculating the average-path-length doesn't really make sense.
  ifelse ( num-connected-pairs != (count turtles * (count turtles - 1) ))
    [ set average-path-length infinity ]
    [ set average-path-length (sum [sum distance-from-other-turtles] of turtles) / (num-connected-pairs) ]

  ; find the clustering coefficient and add to the aggregate for all iterations
  find-clustering-coefficient
end

;;;;; SLUSTERING COMPUTATIONS ;;;;;

to-report in-neighborhood? [ hood ]
  report ( member? end1 hood and member? end2 hood )
end


to find-clustering-coefficient
  ifelse all? turtles [count link-neighbors <= 1]
  [
    ; it is undefined - what should this be?
    set clustering-coefficient 0
  ]
  [
    let total 0
    ask turtles with [ count link-neighbors <= 1] [
      set node-clustering-coefficient "undefined"
    ]
    ask turtles with [ count link-neighbors > 1] [
      let hood link-neighbors
      set node-clustering-coefficient (2 * count links with [ in-neighborhood? hood ] /
                                         ((count hood) * (count hood - 1)) )
      ; find the sum for the value at turtles
      set total total + node-clustering-coefficient
    ]
    ; take the average
    set clustering-coefficient total / count turtles with [count link-neighbors > 1]
  ]
end

;;; PATH LENGTH COMPUTATIONS ;;;

; Implements the Floyd Warshall algorithm for All Pairs Shortest Paths
; It is a dynamic programming algorithm which builds bigger solutions from the solutions of smaller subproblems
; using memoization that is storing the results. It keeps finding incrementally if there is shorter path through
; the kth node. Since it iterates over all turtles through k, so at the end we get the shortest possible path
; for each i and j.

to find-path-lengths
  ; reset the distance list
  ask turtles [
    set distance-from-other-turtles []
  ]

  let i 0
  let j 0
  let k 0
  let node1 one-of turtles
  let node2 one-of turtles
  let node-count count turtles
  ; initialize the distance lists
  while [i < node-count] [
    set j 0
    while [j < node-count] [
      set node1 turtle i
      set node2 turtle j
      ; zero from a node to itself
      ifelse i = j
      [
        ask node1 [
          set distance-from-other-turtles lput 0 distance-from-other-turtles
        ]
      ]
      [
        ; 1 from a node to it's neighbor
        ifelse [ link-neighbor? node1 ] of node2
        [
          ask node1 [
            set distance-from-other-turtles lput 1 distance-from-other-turtles
          ]
        ]
        ; infinite to everyone else
        [
          ask node1 [
            set distance-from-other-turtles lput infinity distance-from-other-turtles
          ]
        ]
      ]
      set j j + 1
    ]
    set i i + 1
  ]
  set i 0
  set j 0
  let dummy 0
  while [k < node-count] [
    set i 0
    while [i < node-count] [
      set j 0
      while [j < node-count] [
        ; alternate path length through kth node
        set dummy ( (item k [distance-from-other-turtles] of turtle i) +
                    (item j [distance-from-other-turtles] of turtle k))
        ;; is the alternate path shorter?
        if dummy < (item j [distance-from-other-turtles] of turtle i)
        [
          ask turtle i [
            set distance-from-other-turtles replace-item j distance-from-other-turtles dummy
          ]
        ]
        set j j + 1
      ]
      set i i + 1
    ]
    set k k + 1
  ]
end

; do-calculations reports true if the network is connected, and reports false if the network is disconnected.
; (In the disconnected case, the average path length does not make sense, or perhaps may be considered infinite)
to-report do-calculations-w-s
  ; set up a variable so we can report if the network is disconnected
  let connected? true

  ; find the path lengths in the network
  find-path-lengths

  let num-connected-pairs sum [length remove infinity (remove 0 distance-from-other-turtles)] of turtles

  ; In a connected network on N nodes, we should have N(N-1) measurements of distances between pairs,
  ; and none of those distances should be infinity.
  ; If there were any "infinity" length paths between nodes, then the network is disconnected.
  ; In that case, calculating the average-path-length doesn't really make sense.
  ifelse ( num-connected-pairs != (count turtles * (count turtles - 1) ))
  [
      set average-path-length infinity
      ; report that the network is not connected
      set connected? false
  ]
  [
    set average-path-length (sum [sum distance-from-other-turtles] of turtles) / (num-connected-pairs)
  ]
  ; find the clustering coefficient and add to the aggregate for all iterations
  find-clustering-coefficient

  ; report whether the network is connected or not
  report connected?
end

; do-calculations-b-a reports true if the network is connected, and reports false if the network is disconnected.
; (In the disconnected case, the average path length does not make sense, or perhaps may be considered infinite)
to do-calculations-b-a
  ; find the path lengths in the network
  find-path-lengths

  let num-connected-pairs sum [length remove infinity (remove 0 distance-from-other-turtles)] of turtles

  ; In a connected network on N nodes, we should have N(N-1) measurements of distances between pairs,
  ; and none of those distances should be infinity.
  ; If there were any "infinity" length paths between nodes, then the network is disconnected.
  ; In that case, calculating the average-path-length doesn't really make sense.
  ifelse ( num-connected-pairs != (count turtles * (count turtles - 1) ))
  [
    set average-path-length infinity
  ]
  [
    set average-path-length (sum [sum distance-from-other-turtles] of turtles) / (num-connected-pairs)
  ]
end

to rewire-one
  ; make sure num-turtles is setup correctly else run setup first
  if count turtles != num-nodes [
    setup
  ]

  ; record which button was pushed
  set rewire-one? true
  set rewire-all? false

  let potential-edges links with [ not rewired? ]
  ifelse any? potential-edges
  [
    ask one-of potential-edges [
      ; "a" remains the same
      let node1 end1
      ; if "a" is not connected to everybody
      if [ count link-neighbors ] of end1 < (count turtles - 1) [
        ; find a node distinct from node1 and not already a neighbor of node1
        let node2 one-of turtles with [ (self != node1) and (not link-neighbor? node1) ]
        ; wire the new edge
        ask node1 [ create-link-with node2 [ set color cyan  set rewired? true ] ]

        set number-rewired number-rewired + 1  ;; counter for number of rewirings

        ; remove the old edge
        die
      ]
    ]
    ; plot the results
    let connected? do-calculations-w-s
    update-plots
  ]
  [ user-message "all edges have already been rewired once" ]
end

to rewire-all
  ; make sure num-turtles is setup correctly; if not run setup first
  if count turtles != num-nodes [
    setup
  ]

  ; record which button was pushed
  set rewire-one? false
  set rewire-all? true

  ; set up a variable to see if the network is connected
  let success? false

  ; if we end up with a disconnected network, we keep trying, because the APL distance
  ; isn't meaningful for a disconnected network.
  repeat num-nodes [
    ; kill the old lattice, reset neighbors, and create new lattice
    ask links [ die ]
    wire-them
    set number-rewired 0

    ask links [
      ; whether to rewire it or not?
      if (random-float 1) < rewiring-probability
      [
        ; "a" remains the same
        let node1 end1
        ; if "a" is not connected to everybody
        if [ count link-neighbors ] of end1 < (count turtles - 1) [
          ; find a node distinct from node1 and not already a neighbor of node1
          let node2 one-of turtles with [ (self != node1) and (not link-neighbor? node1) ]
          ; wire the new edge
          ask node1 [ create-link-with node2 [ set color cyan  set rewired? true ] ]

          set number-rewired number-rewired + 1  ;; counter for number of rewirings
          set rewired? true
        ]
      ]
      ; remove the old edge
      if (rewired?)
      [
        die
      ]
    ]
  ]

  ; do the plotting
  update-plots
end

;;;;; EDGE OPERATIONS ;;;;;

; creates a new lattice
to wire-them
  ; iterate over the turtles
  let n 0
  while [n < count turtles] [
    ; make edges with the next two neighbors
    ; this makes a lattice with average degree of 4
    make-edge turtle n
              turtle ((n + 1) mod count turtles)
    make-edge turtle n
              turtle ((n + 2) mod count turtles)
    set n n + 1
  ]
end

; connects the two turtles
to make-edge [node1 node2]
  ask node1 [ create-link-with node2  [
    set rewired? false
  ] ]
end

to count-degree
  ask turtles [
    set own-degree count link-neighbors
    set is-hub? false
  ]

  set mean-degree mean [own-degree] of turtles * 3
  let replace-hub true
  if hub-strategy = "default" [
    set replace-hub false
  ]

  if replace-hub [
  ask turtles [
      if own-degree > mean-degree [
        set is-hub? true
        set strategy hub-strategy
        set color blue
        setup-set-strategy-label
    ]
   ]
   ]

end
@#$#@#$#@
GRAPHICS-WINDOW
192
59
820
688
-1
-1
20.0
1
10
1
1
1
0
0
0
1
-15
15
-15
15
1
1
1
ticks
8.0

BUTTON
202
18
511
53
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
0

SLIDER
15
87
185
120
initial-wage
initial-wage
50
200
100.0
50
1
NIL
HORIZONTAL

SLIDER
15
124
185
157
cooperation-part
cooperation-part
0
1
0.2
0.01
1
NIL
HORIZONTAL

MONITOR
1026
15
1206
64
Total value
total-company-value
50
1
12

SLIDER
18
659
188
692
mean-value-productivity
mean-value-productivity
2.1
5
3.8
0.05
1
NIL
HORIZONTAL

SLIDER
18
695
188
728
std-deviation-productivity
std-deviation-productivity
0.05
1.5
1.0
0.05
1
NIL
HORIZONTAL

SLIDER
18
739
188
772
mean-value-stress-limit
mean-value-stress-limit
100
500
300.0
10
1
NIL
HORIZONTAL

SLIDER
18
775
188
808
std-deviation-stress-limit
std-deviation-stress-limit
1
10
5.0
1
1
NIL
HORIZONTAL

SLIDER
725
730
895
763
stress-modification-on-PD
stress-modification-on-PD
0
0.1
0.05
0.001
1
NIL
HORIZONTAL

SLIDER
725
694
895
727
stress-regen
stress-regen
0
2
0.325
0.025
1
NIL
HORIZONTAL

BUTTON
519
18
821
53
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
1110
765
1280
798
sick-slider
sick-slider
1
100
7.0
1
1
NIL
HORIZONTAL

PLOT
1459
69
1794
399
profit
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
"default" 1.0 0 -16777216 true "" "plot profit"

SLIDER
192
775
362
808
boss-reaction-time
boss-reaction-time
10
250
130.0
5
1
NIL
HORIZONTAL

SLIDER
368
765
538
798
performance-lower-limit
performance-lower-limit
25
80
50.0
5
1
%
HORIZONTAL

SLIDER
368
799
538
832
performance-upper-limit
performance-upper-limit
110
300
200.0
10
1
%
HORIZONTAL

SLIDER
542
799
712
832
boss-insight-performance
boss-insight-performance
0
1
1.0
0.01
1
NIL
HORIZONTAL

PLOT
1801
405
2176
635
# of patches with low / high own-performance
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
"default" 1.0 0 -8990512 true "" "plot count turtles with [ own-performance <= 1.2 * performance-lower-limit ]"
"pen-1" 1.0 0 -5298144 true "" "plot count turtles with [ own-performance >= 0.80 * performance-upper-limit ]"

SLIDER
18
809
188
842
tft-npm-peace-probability
tft-npm-peace-probability
0
1
0.2
0.01
1
NIL
HORIZONTAL

SLIDER
1110
695
1280
728
patience
patience
0
24
24.0
1
1
NIL
HORIZONTAL

SLIDER
725
764
895
797
effort-stress-increase
effort-stress-increase
0
1
0.5
0.001
1
NIL
HORIZONTAL

SLIDER
542
729
717
762
boss-insight-cooperation-probability
boss-insight-cooperation-probability
0
1
1.0
0.01
1
NIL
HORIZONTAL

MONITOR
1479
15
1685
60
Own-performance-residue
100 * sum [own-performance] of turtles / (count turtles * 100)
17
1
11

PLOT
1026
69
1456
274
stress_sum
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
"default" 1.0 0 -16777216 true "" "plot sum [ stress-actual] of turtles"

TEXTBOX
1319
285
1469
416
black = defect\nred = coop\nyellow = TFT\norange = TFT-npm\nbrown = TF2T\nlila = pavlov\ngreen = unforgiving
13
0.0
1

SLIDER
1110
729
1280
762
sickness-probability
sickness-probability
0
1
1.0
0.1
1
NIL
HORIZONTAL

MONITOR
1206
15
1326
60
NIL
sum count-fired-sick
17
1
11

MONITOR
1326
15
1481
60
NIL
sum count-fired-performance
17
1
11

SLIDER
725
800
895
833
evaluation-stress-change
evaluation-stress-change
0
2
0.9
0.05
1
NIL
HORIZONTAL

TEXTBOX
1926
639
2076
670
blue = low performance\nred  = high performance
13
0.0
1

PLOT
1316
405
1796
830
normalized own-performance per strategy
NIL
performance
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 2 -16777216 true "" "plot sum [ own-performance ] of turtles with [ strategy = \"defect\" ] / max list 1 count turtles with [strategy = \"defect\" ] "
"pen-1" 1.0 2 -2674135 true "" "plot sum [ own-performance ] of turtles with [ strategy = \"cooperate\" ] / max list 1 count turtles with [strategy = \"cooperate\" ] "
"pen-2" 1.0 2 -1184463 true "" "plot sum [ own-performance ] of turtles with [ strategy = \"tit-for-tat\" ] / max list 1 count turtles with [strategy = \"tit-for-tat\" ] "
"pen-3" 1.0 2 -817084 true "" "plot sum [ own-performance ] of turtles with [ strategy = \"tit-for-tat-npm\" ] / max list 1 count turtles with [strategy = \"tit-for-tat-npm\" ] "
"pen-4" 1.0 2 -6459832 true "" "plot sum [ own-performance ] of turtles with [ strategy = \"tit-for-two-tats\" ] / max list 1 count turtles with [strategy = \"tit-for-two-tats\" ] "
"pen-5" 1.0 2 -10141563 true "" "plot sum [ own-performance ] of turtles with [ strategy = \"pavlov\" ] / max list 1 count turtles with [strategy = \"pavlov\" ] "
"pen-6" 1.0 2 -10899396 true "" "plot sum [ own-performance ] of turtles with [ strategy = \"unforgiving\" ] / max list 1 count turtles with [strategy = \"unforgiving\" ] "
"pen-7" 1.0 0 -7500403 true "" "plot-pen-reset\nplotxy 0 100\nplotxy plot-x-max 100"
"pen-8" 1.0 0 -1513240 true "" "plot-pen-reset\nplotxy 0 75\nplotxy plot-x-max 75\n"
"pen-9" 1.0 0 -1513240 true "" "plot-pen-reset\nplotxy 0 125\nplotxy plot-x-max 125"
"pen-10" 1.0 0 -1513240 true "" "plot-pen-reset\nplotxy 0 150\nplotxy plot-x-max 150"

SLIDER
930
700
1100
733
budget-change
budget-change
0
0.1
0.0
0.001
1
NIL
HORIZONTAL

SLIDER
930
770
1102
803
minimal-budget
minimal-budget
0
1
0.1
0.1
1
NIL
HORIZONTAL

SLIDER
930
804
1102
837
maximal-budget
maximal-budget
1
15000
15000.0
0.1
1
NIL
HORIZONTAL

MONITOR
1679
15
1846
60
100 * budget / initial-budget
100 * budget / initial-budget
17
1
11

SLIDER
18
609
188
642
penalisation-for-fluctuation
penalisation-for-fluctuation
0
12
7.0
1
1
NIL
HORIZONTAL

SLIDER
835
90
1005
123
num-nodes
num-nodes
0
100
56.0
1
1
NIL
HORIZONTAL

SLIDER
835
177
1005
210
wiring-probability-inside-community
wiring-probability-inside-community
0
1
0.25
0.01
1
NIL
HORIZONTAL

SLIDER
835
345
1005
378
rewiring-probability
rewiring-probability
0
1
0.09
0.01
1
NIL
HORIZONTAL

BUTTON
835
395
1005
432
NIL
setup-barabasi-albert\n
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
835
306
1004
342
NIL
setup-watts-strogatz
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
1846
15
2176
60
average [own-performance]
sum [own-performance] of turtles / num-nodes
17
1
11

SLIDER
930
734
1100
767
fixed-budget-change
fixed-budget-change
0
0.25
0.0
0.001
1
NIL
HORIZONTAL

PLOT
1801
69
2176
399
# of agents with strategies
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
"default" 1.0 2 -16777216 true "" "plot count turtles with [strategy = \"defect\" ]"
"pen-1" 1.0 2 -2674135 true "" "plot count turtles with [strategy = \"cooperate\" ]"
"pen-2" 1.0 2 -1184463 true "" "plot count turtles with [strategy = \"tit-for-tat\" ]"
"pen-3" 1.0 2 -955883 true "" "plot count turtles with [strategy = \"tit-for-tat-npm\" ]"
"pen-4" 1.0 2 -6459832 true "" "plot count turtles with [strategy = \"tit-for-two-tats\" ]"
"pen-5" 1.0 2 -10141563 true "" "plot count turtles with [strategy = \"pavlov\" ]"
"pen-6" 1.0 2 -10899396 true "" "plot count turtles with [strategy = \"unforgiving\" ]"

PLOT
1026
279
1311
574
normalized number of sick per strategy
NIL
NIL
10.0
100.0
10.0
100.0
true
false
"" ""
PENS
"default" 1.0 2 -16777216 true "" "plot 100 * count turtles with [ is-sick? = true and strategy = \"defect\" ] / max list 1 count turtles with [strategy = \"defect\" ] "
"pen-1" 1.0 2 -5298144 true "" "plot 100 * count turtles  with [ is-sick? = true and strategy = \"cooperate\" ] / max list 1 count turtles with [strategy = \"cooperate\" ] "
"pen-2" 1.0 2 -1184463 true "" "plot 100 * count turtles with [ is-sick? = true and strategy = \"tit-for-tat\" ] / max list 1 count turtles with [strategy = \"tit-for-tat\" ] "
"pen-3" 1.0 2 -14439633 true "" "plot 100 * count turtles with [ is-sick? = true and strategy = \"unforgiving\" ] / max list 1 count turtles with [strategy = \"unforgiving\" ] "
"pen-4" 1.0 2 -7858858 true "" "plot 100 * count turtles with [ is-sick? = true and strategy = \"pavlov\" ] / max list 1 count turtles with [strategy = \"pavlov\" ] "
"pen-5" 1.0 2 -955883 true "" "plot 100 * count turtles with [ is-sick? = true and strategy = \"tit-for-tat-npm\" ] / max list 1 count turtles with [strategy = \"tit-for-tat-npm\" ] "
"pen-6" 1.0 2 -10146808 true "" "plot 100 * count turtles with [ is-sick? = true and strategy = \"tit-for-two-tats\" ] / max list 1 count turtles with [strategy = \"tit-for-two-tats\" ] "

SLIDER
192
809
362
842
max-effort-change
max-effort-change
0
1
0.15
0.05
1
NIL
HORIZONTAL

SLIDER
542
695
719
728
boss-reaction-intensity
boss-reaction-intensity
0
1
0.1
0.01
1
NIL
HORIZONTAL

CHOOSER
192
695
364
740
wage-distribution-strategy
wage-distribution-strategy
"everybody" "increasing-own-perf"
1

SLIDER
542
765
712
798
boss-insight-cooperation-part
boss-insight-cooperation-part
0
1
1.0
0.01
1
NIL
HORIZONTAL

CHOOSER
15
482
187
527
hub-strategy
hub-strategy
"default" "cooperate" "defect" "tit-for-tat" "tit-for-two-tats" "tit-for-tat-npm" "unforgiving" "pavlov"
0

SLIDER
14
222
186
255
defect-strategy
defect-strategy
0
1
0.57
0.01
1
NIL
HORIZONTAL

SLIDER
14
257
186
290
cooperate-strategy
cooperate-strategy
0
1
0.43
0.01
1
NIL
HORIZONTAL

SLIDER
14
327
186
360
tit-for-tat-strategy
tit-for-tat-strategy
0
1
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
14
397
186
430
pavlov-strategy
pavlov-strategy
0
1
0.21
0.01
1
NIL
HORIZONTAL

SLIDER
14
432
187
465
unforgiving-strategy
unforgiving-strategy
0
1
0.47
0.01
1
NIL
HORIZONTAL

SLIDER
14
292
187
325
tit-for-tat-npm-strategy
tit-for-tat-npm-strategy
0
1
0.23
0.01
1
NIL
HORIZONTAL

SLIDER
14
362
187
395
tit-for-two-tats-strategy
tit-for-two-tats-strategy
0
1
0.1
0.01
1
NIL
HORIZONTAL

TEXTBOX
24
64
173
83
Initial Setup
13
0.0
1

TEXTBOX
23
202
177
221
Strategy distribution\n
13
0.0
1

TEXTBOX
840
70
963
89
Topology settings
13
0.0
1

BUTTON
835
441
1005
476
NIL
setup-bianconi-barabasi
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
835
140
1005
173
NIL
setup-stochastic-block
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
1175
585
1305
618
NIL
community-detection
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
835
485
1005
518
NIL
zachary-karate-club\n
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
835
15
1005
60
number of emplyees
count turtles
17
1
11

SLIDER
835
545
1007
578
attrition-percentage
attrition-percentage
0
1
0.08
0.01
1
NIL
HORIZONTAL

SLIDER
835
215
1005
248
wiring-probability-outside-community
wiring-probability-outside-community
0
1
0.14
0.01
1
NIL
HORIZONTAL

SLIDER
835
250
1005
283
number-of-communities
number-of-communities
0
10
6.0
1
1
NIL
HORIZONTAL

CHOOSER
1028
583
1166
628
hiring-strategy
hiring-strategy
"preferential" "random"
0

@#$#@#$#@
# Should the Boss be Nice? How Strategy of Hubs Influence Cooperation and Organizational Performance on Complex Networks 


## WHAT IS IT?

This model represents an organization, such as a company, a university, a factory, etc., where *employees* are provided with a salary to deliver *added value* back to the organization. 

The employees share certain part of their own scope of work with a colleague. From the employee's point of view, the idea behind this concept is that my colleague is better at specific tasks than I am, and vice versa. If we exchange those parts of work then both of us will benefit from that. However, there might be a temptation to betray a colleague and do only mine part of work and benefit from higher performance. The issue of cooperation vs. betrayal is modeled by *Prisoner's Dilemma Game* (henceforth PDG). *(In case you are  not familiar with PDG, take a look at NetLogo Library where you will find several models aimed on this topic.)* 

The concept of stress and sickness is implemented in the model to reflect how "social discomfort" influences the economic performance of the company. 

The model also includes management module that allows to adjust employees' rewarding strategy - whether to increase salary to those who have high performance or to those who are highly cooperative. 

There are several variables in the model that can modify employees' behavior, system properties, management sensitivity, etc. 


## HOW IT WORKS

In our model, employees are described using the following key features that account for their individual properties. 

**Strategy** is a predefined behavior of the employee when PDG is played. For use in our model, we have selected seven simple strategies commonly used in the PDG context, as listed in following Table. The strategies that the employees use are assigned randomly and never change during their lifespan.

```
 Strategy         | Label | Description of behavior                       
 ---------------- | ----- | --------------------------------------------- 
 Defect           |   D   | Always defects.                               
 Cooperate        |   C   | Always cooperates.                            
 Tit for Tat      |   T   | Repeats partner's last move.                  
 Tit for two Tats |   T2  | Defects only if defected in the last 2 rounds 
 Naive Peacemaker |   nT  | Repeats partner's last move. When defected in 
                  |       | the last round, there is a probability of     
                  |       | cooperative response.                         
 Pavlov           |   P   | Starts with cooperation. Repeats action when 
                  |       | won last round and switches action when lost
 Unforgiving      |   U   | Once defected by a partner, it will always
                  |       | respond with a defection. Otherwise cooperates.
```



**Effort** represents an employee's inner motivation to work. While productivity describes the employee's abilities, effort describes how much the employee uses these abilities at a particular moment -- 0\% can be understood as "an employee does not work at all" while 100\% as "an employee does their best".

**Sickness** is a metaphor used to simulate the impact of high stress on working activities of employees. Sick employees do not produce any profit, but the company still supports them financially. Employees that are sick too often are fired and replaced. The tolerance for repeated sick leave is adjustable in the model, common to all employees, and fixed during the simulation.

**Own-performance** illustrate employee's level of performance, where 100 represents standard, implicit 100% peformance, with 50 and 200 being upper and lower limits. The limits may be adjusted by sliders (see Slider section below). 

## HOW TO USE IT

### Buttons

SETUP-REGULAR-LATTICE Initiates the model with a square lattice topology. The number of employees in the model is defined as a squared value of *initial-ws* variable, e.g. *initial-ws* of 15 equals 225 employees. 

**Network models**
Following setup buttons initiate the model and generate topologies specified below. In network models, the number of agents (nodes) equals to attribute *num-nodes*.

SETUP-ERDOS-RENYI 	= Random network model. 
SETUP-WATTS-STROGATS 	= Small-world network model. Topology of this model might be adjusted by two sliders *wiring-probability* and *rewiring-probability*.
SETUP-BARABASI-ALBERT 	= Scale-free network model.

GO: The employees will continuously interact with colleagues to produce value. 

GO ONCE: Same as GO except the employees will only take one step which is representing one day. 

### Sliders

*initial-cooperation* defines the proportion of cooperative strategies (all except "Defect") vs. non-cooperative strategies ("Defect"), i.e., 0.8 = 20 % of "Defect" and 80% of all other strategies.

*cooperation-part* refers to the percentage of the work scope that is exchanged between co-workers, e.g. 20 = 20 % of the total amount of work will be exchanged with a partner for given round. 

*initial-wage* is used to adjust the amount of money that every employee is given at the end of each simulation round.

*patience* define the tolerance for repeated sickness of an employee. After the value is exceeded, then given employees leaves the company and is replaced with a new one. 

*sickness-probability* adjusts the chance that an employee will turn sick when their stress capacity is exceeded. 

*sick-slider* defines the length of sick leave, i.e. how many simulation rounds (days) the employee will stay 'at home'. 

*penalisation-for-fluctuation* Hiring new employees is associated with additional costs 
for the organization (training & lowered performance until it is finished). To simulate these costs, a penalisation for every newcoming employee may be introduced, which is defined as multiplier (defined by the slider) of monthly budget of one employee. 

*budget-change* mimics company growth, i.e., when the company owner decides to return small portion of the total income back to salaries. To prevent growth ad infinitum, minimal and maximal budget variables define lower and upper limits of growth.

*fixed-budget-change* is a similar concept. Insted of redistributing actual budget, a portion of the initial budget is spread among employees. 

*Productivity* 
represents employee's ability to create profit for the company. For sake of simplicity, we assume each employee having fixed productivity during their life. The productivity has normal distribution among the population, with mean value and standard deviation defined by *mean-value-producitity* and *std-deviation-productivity* sliders.

*Resistance to stress* 
is modeled as a capacity of a virtual stress container. If the stress level is below the given  threshold, the employee is healthy and working. When the stress level exceeds the limit, the employee becomes sick and unable to work. When a sick employee returns to work, the stress container is emptied. The stress resistance is also normally distributed among the population, as defined by sliders *mean-value-stress-limit* and *std-deviation-stress-limit*.

*Stress*:
There are several ways of how employees' stress is produced. 
1) Social interaction - stress caused by defection in PDG, adjusted by *stress-modification-on-PD* slider. 
2) Internal factors - stress caused by level of effort (slider *effort-stress-increase*) and stress caused by evaluation by management (slider *evaluation-stress-change*).
On the other hand, the stress level can also be lowered by *stress-regen* slider. 


*Management decision module*: 
When management evaluates individuals' performance to decide how to distribute the budget accordingly (to reward highly performing individuals and punish low performers), they may also consider one's cooperativeness as well. Whenever the cooperating employee is being defected, he or she looses part of the work done (as specified by *coop-part* slider).

*boss-insight-cooperation-probability* represent a chance that the management will be aware of this fact and will reimburse the value lost when cooperative agent was defected. Value of 0.7 means that in 7 out of 10 acts of defection, the management will take it into consideration. 
 
*boss-insight-cooperation-part* represents the extent of how much the lost value will be reimbursed. Value of 0.7 means that 70 % of the lost value will be returned into calculation for next month budget distribution of given cooperative employee. 

*boss-reaction-intensity* adjusts the intensity of impact on salary after an employee is evaluated. For example 0.15 means that the management can only adjust 15% of the total salary. In other words, 85% of the salary is fixed. 

*upper and lower performance limits* define maximal and minimal performance of one employee. Given that management evaluation has (de)motivational effects, the performance of an employee will rise or fall. However, this cannot repeat ad infinitum. These limits ensure that employees cannot drop under 50% or exceed 200% performance. 

*max-effort-change* specifies what it the maximal allowed change in individual's motivation after the budget is redistributed. 

### Drop-down list
*hub-strategy* allows to change strategy of highly central nodes (hubs), representing managers or bosses inside the organization. *Default* selection keeps the strategy randomly assigned by the setup algorithms, all other selections force hubs to adopt selected strategy. 

### Switches

*retirement*: Turn on and off the possibility to replace employees when they are repeatedly sick.  

*fixed-performance* and *effort-change*: On by default. Might be turned off to nulify the motivational effects on employees performance.


## HOW TO REPLICATE THE EXPERIMENT

The model was upload to CoMSES Net Computational Model Library with the experimental setup prepared in the BehaviorSpace tool. To access the BehaviorSpace, go to Tools>BehaviorSpace or press Control+Shift+B. Then Click "RUN" and select output format. Both Table and Spreadsheet will produce csv file, which is to be imported to preffered data processing tool.  


## THINGS TO NOTICE

### Impact of Management Strategies on Profit

- Focusing on cooperativeness and a sufficiently deep insight into the cooperation result in the highest value produced by an organization.
- The strategy based mostly on cooperativeness can achieve an approximately 80-90\% performance level compared to the best result, as long as the insight for cooperation is also high enough.
- The lowest observed performance (below 10\% compared to the best result) is achieved by an organization where the management has very low or zero insight into both cooperation and performance regardless of which aspect of the work is preferred. Such a setting creates a favorable environment for agents with defective strategies whose population fluctuate around 30-40\% of the total agent population, compared to the initial 20\%.    

### Impact of Management Strategies on Stress

The rate of sickness is high when the management strategy is focused on rewarding performance while the insight for performance is low, or when the strategy aims at rewarding cooperation and the insight for cooperation is low. In other words, the two above-mentioned rewarding strategies exhibit high levels of randomness, resulting in a high level of stress among employees. See Figure~\ref{fig:scatter-plot-sickness} for details.

### Impact of Management Strategies on Employment Fluctuation

The fluctuation is calculated as a total count of employees that leave the organization, either due to repeated sickness or to long-term underperformance. 

A strategy of rewards that aims to improve performance causes high fluctuation when the insight for performance is also high.

## EXTENDING THE MODEL

The model can be further extended by implementing a social network model where the interaction of employees would be more complex, influenced by the topology of a network, and exploring the potential impact of more central agents on the overall performance of the organization. 

There are several other suggestions for future versions of the model, e.g. introduction of organizational growth to reflect the economic prosperity of the company, implementing more realistic model of employees' productivity, involving personality types to fit specific organizations, or to focus on space and distance between employees that has an impact on quality of interaction.

Finally, the employment fluctuation algorithm can be further developed. It should be possible to relocate the employees or change their collaboration strategy before they leave the organization. And a higher penalization of a company for repeatedly dismissing employees could be implemented to better simulate a situation where there is low unemployment in the job market.

## CREDITS

### AUTHORS

Josef Spurný, 274152@mail.muni.cz
Ivan Kopeček, kopecek@fi.muni.cz
Radek Ošlejšek, oslejsek@fi.muni.cz
Jaromír Plhák, xplhak@fi.muni.cz


### INSTITUTE

[Laboratory of Speech and Dialogue, Faculty of Informatics, Masaryk University, Brno, Czech Republic](http://lsd.fi.muni.cz)

## REFERENCES

Simulating the Impact of Cooperation and Management Strategies on Stress and Economic Performance. DAŇA, Josef, Ivan KOPEČEK, Radek OŠLEJŠEK a Jaromír PLHÁK. Simulating the Impact of Cooperation and Management Strategies on Stress and Economic Performance. In Proceedings of the 52nd Hawaii International Conference on System Sciences. To appear. USA, 2019. 10 s.


## CHANGELOG

v1.0 (09/2018): First workable version with basic functionality



<!-- 2017-2018 -->
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

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="should_be_boss_nice" repetitions="500" runMetricsEveryStep="false">
    <setup>setup-barabasi-albert</setup>
    <go>go</go>
    <timeLimit steps="3600"/>
    <metric>total-company-value</metric>
    <metric>sum [own-performance] of turtles / num-nodes</metric>
    <metric>sum [sick-counter] of turtles</metric>
    <metric>sum [ own-performance ] of turtles with [ strategy = "defect" ] / max list 1 count turtles with [strategy = "defect" ]</metric>
    <metric>sum [ own-performance ] of turtles with [ strategy = "cooperate" ] / max list 1 count turtles with [strategy = "cooperate" ]</metric>
    <metric>sum [ own-performance ] of turtles with [ strategy = "tit-for-tat" ] / max list 1 count turtles with [strategy = "tit-for-tat" ]</metric>
    <metric>sum [ own-performance ] of turtles with [ strategy = "tit-for-two-tats" ] / max list 1 count turtles with [strategy = "tit-for-two-tats" ]</metric>
    <metric>sum [ own-performance ] of turtles with [ strategy = "tit-for-tat-npm" ] / max list 1 count turtles with [strategy = "tit-for-tat-npm" ]</metric>
    <metric>sum [ own-performance ] of turtles with [ strategy = "pavlov" ] / max list 1 count turtles with [strategy = "pavlov" ]</metric>
    <metric>sum [ own-performance ] of turtles with [ strategy = "unforgiving" ] / max list 1 count turtles with [strategy = "unforgiving" ]</metric>
    <enumeratedValueSet variable="hub-strategy">
      <value value="&quot;cooperate&quot;"/>
      <value value="&quot;defect&quot;"/>
      <value value="&quot;tit-for-tat&quot;"/>
      <value value="&quot;tit-for-two-tats&quot;"/>
      <value value="&quot;tit-for-tat-npm&quot;"/>
      <value value="&quot;pavlov&quot;"/>
      <value value="&quot;unforgiving&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring-probability">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maximal-budget">
      <value value="100000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wage-distribution-strategy">
      <value value="&quot;increasing-own-perf&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="effort-change">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evaluation-stress-change">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="boss-insight-cooperation-probability">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="boss-reaction-intensity">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stress-regen">
      <value value="1.125"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="penalisation-for-fluctuation">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sickness-probability">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="boss-insight-performance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-ws">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wiring-prob">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="retirement">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stress-modification-on-PD">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="budget-change">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimal-budget">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="boss-insight-cooperation-part">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="performance-lower-limit">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="boss-reaction-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cooperation-part">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixed-budget-change">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="std-deviation-productivity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patience">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="performance-upper-limit">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-wage">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="std-deviation-stress-limit">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-cooperation">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="effort-stress-increase">
      <value value="0.525"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sick-slider">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-value-productivity">
      <value value="3.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixed-performance">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tft-npm-peace-probability">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-effort-change">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-value-stress-limit">
      <value value="300"/>
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
