defmodule FIX.MixProject do
  use Mix.Project

  def project do
    [
      app:             :fix,
      version:         "1.0.0",
      elixir:          "~> 1.13",
      start_permanent: Mix.env() == :prod,
      deps:            deps(),
      elixirc_paths:   ["src"],
    ]
  end

  def application do
    [
      extra_applications: [:logger],
    ]
  end

  defp deps do
    [
      {:parse_trans, "~> 3.3.1", runtime: false},
      {:etran,       "~> 0.3",   runtime: false},
      {:util,        "~> 1.0"},
    ]
  end
end
