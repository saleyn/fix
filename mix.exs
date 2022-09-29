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
      compilers:       [:priv] ++ Mix.compilers ++ [:escript],
      language:        :erlang,
      #escript:         escript(),
      #aliases:         [compile: ["compile", "escript.build"]]
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

  #defp escript() do
  #  outdir  = Keyword.get(Mix.Project.config(), :app_path, File.cwd!)
  #  escript = System.get_env("ESCRIPT",   "escript")
  #  args    = System.get_env("FIXDUMP_ENV",      "")
  #  [
  #    main_module:  :fix_dump,
  #    #app:          :fix,
  #    path:         Path.join(outdir, "priv/fixdump"),
  #    shebang:      "#!/usr/bin/env #{escript}\n",
  #    comment:      "vim:sw=2:ts=2:et",
  #    emu_args:     "-env ERL_CRASH_DUMP /dev/null #{args}",
  #    #embed_elixir: false,
  #  ]
  #end
end

## We use a custom script building module instead of Elixir's
## escript task because we only want two modules included in the
## escript rather than the entire :fix, and :util apps.
defmodule Mix.Tasks.Compile.Escript do
  use Mix.Task.Compiler
  import Bitwise, only: [|||: 2]

  @impl true
  def run(_args) do
    {:ok, [:util, :fix]} = Application.ensure_all_started(:fix)
    files = for f <- [:fix_dump, :env] do
      ff = :code.which(f)
      {:filename.basename(ff), File.read!(ff)}
    end
    outdir   = Keyword.get(Mix.Project.config(), :app_path, File.cwd!)
    escript  = System.get_env("ESCRIPT",   "escript")
    filename = Path.join(outdir, "priv/fixdump")
    args     = System.get_env("FIXDUMP_ENV", "")
    :ok      = filename |> String.to_charlist |> :escript.create([
      {:shebang,   "/usr/bin/env #{escript}" |> to_charlist},
      {:comment,   'vim:sw=2:ts=2:et'},
      {:emu_args,  "-escript main fix_dump -env ERL_CRASH_DUMP /dev/null #{args}" |> to_charlist},
      {:archive,   files, []},
    ])
    stat = File.stat!(filename)
    :ok  = File.chmod(filename, stat.mode ||| 0o111)
    IO.puts("Created #{filename}")
  end
end

defmodule Mix.Tasks.Compile.Priv do
  use Mix.Task.Compiler

  #@fixdump "priv/fixdump.es"

  @impl true
  def run(_args) do
    outdir = Keyword.get(Mix.Project.config(), :app_path, File.cwd!)

    {result, _errcode} = System.cmd("make", ["nif"], [env: [{"REBAR_BARE_COMPILER_OUTPUT_DIR", outdir}]])
    IO.binwrite(result)

    #val  = System.get_env("FIXDUMP_ENV",      "")
    #escr = System.get_env("ESCRIPT",   "escript")
    #body = File.read!("src/fixdump.es.src")
    #    |> String.replace("{{ESCRIPT}}",    escr)
    #    |> String.replace("{{FIXDUMP_ENV}}", val)
    #:ok  = File.write!(@fixdump,  body)
    #:ok  = File.chmod!(@fixdump, 0o755)
    #IO.puts("Copied src/fixdump.es.src -> " <> @fixdump)

    #dest = Path.join(outdir, @fixdump)
    #if System.cmd("readlink", ["-f", @fixdump]) != System.cmd("readlink", ["-f", dest]) do
    #  :ok = File.cp!(@fixdump, dest)
    #  IO.puts("Copied #{@fixdump} -> #{dest}")
    #end

    #:ok
  end

  @impl true
  def clean() do
    files  = File.rm_rf!("priv") ++
             (Path.wildcard("c_src/*.o") |> Enum.filter(& File.rm(&1) == :ok))
    files != [] && IO.puts("Deleted #{inspect(files)}")

    outdir = Keyword.get(Mix.Project.config(), :app_path)
    outdir && Path.join(outdir, "priv") |> File.rm_rf! |> then(& (&1 != [] && IO.puts("Deleted #{inspect(&1)}")))
  end
end
