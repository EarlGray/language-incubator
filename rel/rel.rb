# See : http://codon.com/hello-declarative-world
# A tiny relational DSL inside Ruby
#
# Example of usage:
#
# # Prolog: X = Y, Y = 5.
# irb> load "rel.rb"
# irb> goal = Goal.with_variables { |x, y| Goal.both(Goal.equal(x, y), Goal.equal(y, 5)) }
# irb> states = goal.pursue_in State.new
# irb> states.first.values
# => {x=>y, y=>5}
#
#
# # Prolog: {X, 3} = {4, Y}.
# irb> goal = Goal.with_variables { |x, y| Goal.equal(Pair.new(x, 3), Pair.new(4, y)) }
# irb> states = goal.pursue_in State.new
# irb> states.first.values
# => {x=>4, y=>3}
#
# # Prolog: [X, 2, Z] = [1, Y, 3].
# irb> goal = Goal.with_variables { |x, y, z| Goal.equal(to_list [x, 2, z], to_list [1, y, 3]) }
# irb> states = goal.pursue_in State.new
# irb> states.first.values
# => {x=>1, y=>2, z=>3}
#
# # Prolog: add(X, Y, 4).
# irb> goal = Goal.with_variables { |x, y| add(x, y, to_peano(4)) }
# irb> states = goal.pursue_in State.new
# irb> states.first.results(2).map { |x| from_peano(x) }
# => [0, 4]
# irb> states.next.results(2).map { |x| from_peano(x) }
# => [1, 3]
# irb> states.next.results(2),map { |x| from_peano(x) }
# => [2, 2]
# ...

class Variable
  def initialize(name)
    @name = name
  end

  def inspect
    @name
  end
end

class State
  attr_reader :variables, :values

  def initialize(variables = [], values = {})
    @variables, @values = variables, values
  end

  def create_variables(names)
    new_variables = names.map { |name| Variable.new(name) }
    [
      State.new(@variables + new_variables, @values),
      new_variables
    ] 
  end

  def assign_values(new_values)
    State.new(@variables, @values.merge(new_values))
  end

  def value_of(key)
    if @values.has_key?(key)
      value_of @values.fetch(key)
    elsif key.is_a?(Pair)
      Pair.new(
        value_of(key.left),
        value_of(key.right)
      )
    else
      key
    end
  end

  def unify(a, b)
    a, b = value_of(a), value_of(b)

    if a == b
      self
    elsif a.is_a?(Variable)
      assign_values a => b
    elsif b.is_a?(Variable)
      assign_values b => a
    elsif a.is_a?(Pair) && b.is_a?(Pair)
      state = unify(a.left, b.left)
      state.unify(a.right, b.right) if state
    end
  end

  def results(n)
    variables.first(n).
      map { |variable| value_of(variable) }
  end

  def result
    results(1).first
  end
end

class Goal
  def initialize(&block)
    @block = block
  end

  def pursue_in(state)
    @block.call state
  end

  def pursue_in_each(states)
    Enumerator.new do |yielder|
      first, remaining = states.next, states

      first_stream, remaining_streams = 
        pursue_in(first), pursue_in_each(remaining)

      first_stream.interleave_with(remaining_streams).each do |state|
        yielder.yield state
      end
    end
  end
end

def Goal.with_variables(&block)
  names = block.parameters.map { |type, name| name }

  Goal.new do |state|
    state, variables = state.create_variables(names)
    goal = block.call(*variables)
    goal.pursue_in state
  end
end

def Goal.equal(a, b)
  Goal.new do |state|
    state = state.unify(a, b)

    Enumerator.new do |yielder|
      yielder.yield state if state
    end
  end
end

def Goal.either(first_goal, second_goal)
  Goal.new do |state|
    first_stream = first_goal.pursue_in(state)
    second_stream = second_goal.pursue_in(state)

    first_stream.interleave_with(second_stream)
  end
end

def Goal.both(first_goal, second_goal)
  Goal.new do |state|
    states = first_goal.pursue_in(state)
    second_goal.pursue_in_each(states)
  end
end

class Enumerator
  def interleave_with(other)
    enumerators = self, other
    
    Enumerator.new do |yielder|
      until enumerators.empty?
        loop do
          enumerator = enumerators.shift
          yielder.yield enumerator.next
          enumerators.push enumerator
        end
      end
    end
  end
end

Pair = Struct.new(:left, :right) do
  def inspect
    "(#{left.inspect}, #{right.inspect})"
  end
end

EMPTY_LIST = :empty

def to_list(array)
  if array.empty?
    EMPTY_LIST
  else
    first, *rest = array
    Pair.new(first, to_list(rest))
  end
end

def from_list(list)
  if list == EMPTY_LIST
    []
  else
    first, rest = list.left, list.right
    [first, *from_list(rest)]
  end
end

def append(a, b, c)
  Goal.either(
    Goal.both(
      Goal.equal(a, EMPTY_LIST),
      Goal.equal(b, c)
    ),
    Goal.with_variables { |first, rest_of_a, rest_of_c|
      Goal.both(
        Goal.both(
          Goal.equal(a, Pair.new(first, rest_of_a)),
          Goal.equal(c, Pair.new(first, rest_of_c))
        ),
        append(rest_of_a, b, rest_of_c) 
      )
    }
  )
end

ZERO, INC = :z, :+

def to_peano(number)
  if number.zero?
    ZERO
  else 
    Pair.new(INC, to_peano(number - 1))
  end
end

def from_peano(peano)
  if peano == ZERO
    0
  else
    1 + from_peano(peano.right)
  end
end

# %% Prolog:
# add(o, Y, Z) :- Y = Z;
# add(s(X), Y, s(Z)) :- add(X, Y, Z).
def add(x, y, z)
  Goal.either(
    Goal.both(
      Goal.equal(x, ZERO),
      Goal.equal(y, z)
    ),
    Goal.with_variables { |smaller_x, smaller_z|
      Goal.both(
        Goal.both(
          Goal.equal(x, Pair.new(INC, smaller_x)),
          Goal.equal(z, Pair.new(INC, smaller_z))
        ),
        add(smaller_x, y, smaller_z)
      ) 
    }
  )
end
