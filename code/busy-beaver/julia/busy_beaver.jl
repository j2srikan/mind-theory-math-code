# initial values for tape/position/state
tape = [0]
pos = 1
state = 'a'

# 2-state rules
rules2 = Dict{String, Any}("a0" => ('b', 1, 'r'), "a1" => ('b', 1, 'l'), 
                           "b0" => ('a', 1, 'l'), "b1" => ('h', 1, 'r'))

# 5-state rules
rules5 = Dict{String, Any}("a0" => ('b', 1, 'r'), "a1" => ('e', 1, 'l'), 
                           "b0" => ('c', 1, 'r'), "b1" => ('a', 1, 'l'),
                           "c0" => ('d', 1, 'r'), "c1" => ('d', 1, 'l'),
                           "d0" => ('c', 1, 'l'), "d1" => ('b', 1, 'l'),
                           "e0" => ('e', 1, 'r'), "e1" => ('h', 1, 'r'))

# identify state and direction
function read_state(rules)
    # print state/tape/position
    println(string("state: $state, ", "tape: $tape, ", "position: $pos"))

    # gather state and shift indicators from rules
    state_tag = string(state, tape[pos])
    global state = rules[state_tag][1]
    global tape_write = rules[state_tag][2]
    global shift = rules[state_tag][3]
end

# read direction to shift & update tape 
function shift_pos()
    # write to tape
    tape[pos] = tape_write

    # shift position to right
    if shift == 'r'
        global pos += 1
        # add position to right if outside of boundary
        if pos > length(tape)
            push!(tape, 0)
        end

    # shift position to left
    elseif shift == 'l'
        pos -= 1
        # add position to left if outside of boundray
        if pos < 1
            pushfirst!(tape, 0)
            pos += 1
        end
    end
end

function run_beaver(rules)
    # run turing machine until halt is reached
    while state !== 'h'
        read_state(rules)
        shift_pos()
    end

    # print halt message
    println(string("state: halt, ", "tape: $tape, ", "position: $pos"))

    # sum all the 1s created by busy beaver
    ones = sum(tape)
    println(string("The busy beaver wrote $ones 1s"))
end

# run_beaver(rules2) for the 2-state busy beaver
run_beaver(rules5)