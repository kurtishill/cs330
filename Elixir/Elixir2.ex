defmodule NameServer do
  use GenServer

  # Start Helper Functions (Don't Modify)
  def start_link() do
    GenServer.start_link(__MODULE__, [], [])
  end

  def start() do
    GenServer.start(__MODULE__, [],  [])
  end

  def register(name_server, name) do
    GenServer.call(name_server, {:register, name})
  end

  def register(name_server, name, pid) do
    GenServer.cast(name_server, {:register, name, pid})
  end

  def resolve(name_server, name) do
    GenServer.call(name_server, {:resolve, name})
  end
  #End Helper Functions

  def init(_) do
    {:ok, Map.new()}
  end

  def handle_call({:register, name}, {pid, _from}, map) do
    newMap = Map.put(map, name, pid)
    {:reply, :ok, newMap}
  end

  def handle_call({:resolve, name}, _sender, map) do
    p = Map.fetch(map, name)
    case p do
      :error -> {:reply, :error, map}
      {:ok, pid} -> {:reply, pid, map}
    end
  end

  def handle_cast({:register, name, pid}, map) do
    newMap = Map.put(map, name, pid)
    {:noreply, newMap}
  end


  def handle_call(request, from, state) do
    super(request, from, state)
  end

  def handle_cast(request, state) do
    super(request, state)
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end
end # end defmodule NameServer


defmodule TopSupervisor do
  use Supervisor

  def start_link(ns) do
    Supervisor.start_link(__MODULE__, ns)
  end

  def init(ns) do
    children = [
      worker(DatabaseSupervisor, [ns]),
      worker(CustomerService, [ns])
    ]
    supervise(children, strategy: :one_for_one)
  end
end # end TopSupervisor

defmodule DatabaseSupervisor do
  use Supervisor

  def start_link(ns) do
    Supervisor.start_link(__MODULE__, ns)
  end

  def init(ns) do
    children = [
      worker(Database, [ns]),
      worker(DepartmentSupervisor, [ns])
    ]

    supervise(children, strategy: :rest_for_one)
  end
end # end DatabaseSupervisor

defmodule DepartmentSupervisor do
  use Supervisor

  def start_link(ns) do
    Supervisor.start_link(__MODULE__, ns)
  end

  def init(ns) do
    children = [
      worker(Info, [ns]),
      worker(Shipper, [ns]),
      worker(UserOrderSupervisor, [ns])
    ]

    supervise(children, strategy: :one_for_one)
  end
end # end DepartmentSupervisor

defmodule UserOrderSupervisor do
  use Supervisor

  def start_link(ns) do
    Supervisor.start_link(__MODULE__, ns)
  end

  def init(ns) do
    children = [
      worker(User, [ns]),
      worker(Order, [ns])
    ]

    supervise(children, strategy: :one_for_all)
  end
end # end UserOrderSupervisor


defmodule Database do
  use GenServer

  def start_link(name_server, opts \\ []) do
    GenServer.start_link(__MODULE__, name_server, [])
  end

  def start(name_server) do
    GenServer.start(__MODULE__, name_server,  [])
  end

  def call(mod) do
    GenServer.call(mod, "Message")
  end

  def kill(mod) do
    Process.exit(mod, "killed")
  end

  def init(ns) do
    # DB has no Depends
    IO.puts Atom.to_string(__MODULE__) <> " has started"
    NameServer.register(ns, :Database, self())
    {:ok, []}
  end

  def handle_call(_request, _From, state) do
    #check Depends
    {:reply, :ok, state}
  end


  def handle_cast(_request, state) do
    #check Depends (none)
    {:noreply, state}
  end

  def handle_info(_, state) do
    #checkDepends (none)
    {:noreply, state}
  end
end # end defmodule Database




defmodule CustomerService do
  use GenServer

  def start_link(name_server, opts \\ []) do
    GenServer.start_link(__MODULE__, name_server, [])
  end

  def start(name_server) do
    GenServer.start(__MODULE__, name_server,  [])
  end

  def call(mod) do
    GenServer.call(mod, "Message")
  end

  def kill(mod) do
    Process.exit(mod, "killed")
  end

  def init(ns) do
    # DB has no Depends
    IO.puts Atom.to_string(__MODULE__) <> " has started"
    NameServer.register(ns, :CustomerService, self())
    {:ok, []}
  end

  def handle_call(_request, _From, state) do
    #check Depends
    {:reply, :ok, state}
  end

  def handle_cast(_request, state) do
    #check Depends (none)
    {:noreply, state}
  end

  def handle_info(_, state) do
    #checkDepends (none)
    {:noreply, state}
  end
end # end defmodule CustomerService


defmodule Info do
  use GenServer

  def start_link(name_server, opts \\ []) do
    GenServer.start_link(__MODULE__, name_server, [])
  end

  def start(name_server) do
    GenServer.start(__MODULE__, name_server,  [])
  end

  def call(mod) do
    GenServer.call(mod, "Message")
  end

  def kill(mod) do
    Process.exit(mod, "killed")
  end

  def init(ns) do
    # DB has no Depends
    IO.puts Atom.to_string(__MODULE__) <> " has started"
    NameServer.register(ns, :Info, self())
    send self(), {:start, ns}
    {:ok, []}
  end

  def handle_call(_request, _From, state) do
    if ! check_alive(state) do
      {:stop, "dependency not found", "dependency not found", state}
    end
    {:reply, :ok, state}
  end

  def check_alive(processes) do
    x = Enumerable.map(processes, &Process.alive/0)
    reducer =  fn l, r -> l and r end
    Enumberable.reduce(x, true, reducer)
  end

  def handle_cast(_request, state) do
    #check Depends (none)
    {:noreply, state}
  end

  def handle_info({:start, ns}, _state) do
    :timer.sleep(500)
    case NameServer.resolve(ns, :Database) do
      :error -> {:stop, "No Database Found", []}
      x -> {:noreply, [x]}
    end
  end

  def handle_info(_, state) do
    #checkDepends (none)
    {:noreply, state}
  end
end # end defmodule Info

defmodule Order do
  use GenServer

  def start_link(name_server, opts \\ []) do
    GenServer.start_link(__MODULE__, name_server, [])
  end

  def start(name_server) do
    GenServer.start(__MODULE__, name_server,  [])
  end

  def call(mod) do
    GenServer.call(mod, "Message")
  end

  def kill(mod) do
    Process.exit(mod, "killed")
  end

  def init(ns) do
    # DB has no Depends
    IO.puts Atom.to_string(__MODULE__) <> " has started"
    NameServer.register(ns, :Order, self())
    send self(), {:start, ns}
    {:ok, []}
  end

  def handle_call(_request, _From, state) do
    if ! check_alive(state) do
      {:stop, "dependency not found", "dependency not found", state}
    end
    {:reply, :ok, state}
  end

  def check_alive(processes) do
    x = Enumerable.map(processes, &Process.alive/0)
    reducer = fn l, r -> l and r end
    Enumberable.reduce(x, true, reducer)
  end

  def handle_cast(_request, state) do
    #check Depends (none)
    {:noreply, state}
  end

  def handle_info({:start, ns}, _state) do
    :timer.sleep(500)
    case NameServer.resolve(ns, :Database) do
      :error -> {:stop, "No Database Found", []}
      x ->
        case NameServer.resolve(ns, :User) do
          :error -> {:stop, "User Module not Found", []}
          y -> {:noreply, [x, y]}
        end
    end
  end

  def handle_info(_, state) do
    #checkDepends (none)
    {:noreply, state}
  end
end # end defmodule Order

defmodule Shipper do
  use GenServer

  def start_link(name_server, opts \\ []) do
    GenServer.start_link(__MODULE__, name_server, [])
  end

  def start(name_server) do
    GenServer.start(__MODULE__, name_server,  [])
  end

  def call(mod) do
    GenServer.call(mod, "Message")
  end

  def kill(mod) do
    Process.exit(mod, "killed")
  end

  def init(ns) do
    # DB has no Depends
    IO.puts Atom.to_string(__MODULE__) <> " has started"
    NameServer.register(ns, :Shipper, self())
    send self(), {:start, ns}
    {:ok, []}
  end

  def handle_call(_request, _From, state) do
    if ! check_alive(state) do
      {:stop, "dependency not found", "dependency not found", state}
    end
    {:reply, :ok, state}
  end

  def check_alive(processes) do
    x = Enumerable.map(processes, &Process.alive/0)
    reducer = fn l, r -> l and r end
    Enumberable.reduce(x, true, reducer)
  end

  def handle_cast(_request, state) do
    #check Depends (none)
    {:noreply, state}
  end

  def handle_info({:start, ns}, _state) do
    :timer.sleep(500)
    case NameServer.resolve(ns, :Database) do
      :error -> {:stop, "No Database Found", []}
      x ->{:noreply, [x]}

    end
  end

  def handle_info(_, state) do
    #checkDepends (none)
    {:noreply, state}
  end
end # end defmodule Shipper

defmodule User do
  use GenServer

  def start_link(name_server, opts \\ []) do
    GenServer.start_link(__MODULE__, name_server, [])
  end

  def start(name_server) do
    GenServer.start(__MODULE__, name_server,  [])
  end

  def call(mod) do
    GenServer.call(mod, "Message")
  end

  def kill(mod) do
    Process.exit(mod, "killed")
  end

  def init(ns) do
    # DB has no Depends
    IO.puts Atom.to_string(__MODULE__) <> " has started"
    NameServer.register(ns, :User, self())
    send self(), {:start, ns}
    {:ok, []}
  end

  def handle_call(_request, _From, state) do
    if ! check_alive(state) do
      {:stop, "dependency not found", "dependency not found", state}
    end
    {:reply, :ok, state}
  end

  def check_alive(processes) do
    x = Enumerable.map(processes, &Process.alive/0)
    reducer = fn l, r -> l and r end
    Enumberable.reduce(x, true, reducer)
  end

  def handle_cast(_request, state) do
    #check Depends (none)
    {:noreply, state}
  end

  def handle_info({:start, ns}, _state) do
    :timer.sleep(500)
    case NameServer.resolve(ns, :Database) do
      :error -> {:stop, "No Database Found", []}
      x ->
        case NameServer.resolve(ns, :Order) do
          :error -> {:stop, "User Module not Found", []}
          y -> {:noreply, [x, y]}
        end
    end
  end

  def handle_info(_, state) do
    #checkDepends (none)
    {:noreply, state}
  end
end # end defmodule User

defmodule Crasher do

  def crash(ns, name) do
    IO.puts("Crashing the module...")
    pid = GenServer.call(ns, {:resolve, name})
    if pid == :error do
	IO.puts(["Unable to find process ", Atom.to_string(name)])
    else
	Process.exit(pid, :kill)
    end
  end
end # end defmodule Crasher
