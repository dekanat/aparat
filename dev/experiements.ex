defmodule SeaC.MiscTest do
  alias SeaC.Evaluator
  use ExUnit.Case

  @tag one: true
  test "asd" do
    bet = 60
    roll_the_dice = fn e -> {:rand.uniform(6), :rand.uniform(6)} end

    calculator = fn round, acc ->
      coefficient =
        case round do
          {1, 1} -> 1
          {2, 2} -> 2
          {3, 3} -> 3
          {4, 4} -> 4
          {5, 5} -> 5
          {6, 6} -> 6
          _ -> -1
        end

      acc + coefficient * bet
    end

    arrange_wins = fn balance, {player, casino} ->
      cond do
        balance < 0 -> {player, casino + -balance}
        balance > 0 -> {player + balance, casino}
        true -> {player, casino}
      end
    end

    simulate_game = fn rounds ->
      Enum.to_list(1..200)
      |> Enum.map(roll_the_dice)
      |> List.foldl(0, calculator)
    end

    simulate_many = fn games ->
      Enum.to_list(1..games)
      |> Enum.map(simulate_game)
    end

    pnduk =
      simulate_many.(50)
      |> List.foldl({0, 0}, arrange_wins)

    {p, c} = pnduk

    IO.inspect(pnduk)
    IO.inspect(p / (p + c))
    IO.inspect(c / (p + c))

    assert p / (p + c) < c / (p + c)
  end
end
