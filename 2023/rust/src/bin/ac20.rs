use advent_of_code::{Result, NameTable, Name, NameSet, NameMap, lcm};

#[derive(Copy, Clone, PartialEq, Eq)]
enum Pulse { Low, High }

trait Module {
    fn add_input(&mut self, input: Name);

    fn get_inputs(&self) -> Option<&NameSet>;

    fn run(&self) -> Box<dyn RunningModule>;
}

trait RunningModule {
    fn receive(
        &mut self, message: &Message, send: &mut dyn FnMut(Pulse)
    );
}

struct FlipFlop {}

struct RunningFlipFlop {
    state: bool,
}

struct Conjunction {
    inputs: NameSet,
}

impl Conjunction {
    fn new() -> Self {
        Self { inputs: NameSet::new() }
    }
}

struct RunningConjunction {
    low_set: NameSet,
}

struct Broadcaster {}

impl Module for FlipFlop {
    fn add_input(&mut self, _input: Name) {}

    fn get_inputs(&self) -> Option<&NameSet> {
        None
    }

    fn run(&self) -> Box<dyn RunningModule> {
        Box::new(RunningFlipFlop { state: false })
    }
}

impl RunningModule for RunningFlipFlop {
    fn receive(&mut self, message: &Message, send: &mut dyn FnMut(Pulse)) {
        if message.pulse == Pulse::Low {
            send(if self.state { Pulse::Low } else { Pulse::High });
            self.state = !self.state;
        }
    }
}

impl Module for Conjunction {
    fn add_input(&mut self, input: Name) {
        self.inputs.insert(input);
    }

    fn get_inputs(&self) -> Option<&NameSet> {
        Some(&self.inputs)
    }

    fn run(&self) -> Box<dyn RunningModule> {
        Box::new(RunningConjunction { low_set: self.inputs.clone() })
    }
}

impl RunningModule for RunningConjunction {
    fn receive(&mut self, message: &Message, send: &mut dyn FnMut(Pulse)) {
        match (message.pulse, self.low_set.contains(&message.input)) {
            (Pulse::Low, false) => { self.low_set.insert(message.input); }
            (Pulse::High, true) => { self.low_set.remove(&message.input); }
            (Pulse::Low, true) | (Pulse::High, false) => (),
        };
        send(if self.low_set.is_empty() { Pulse::Low } else { Pulse :: High })
    }
}

impl Module for Broadcaster {
    fn add_input(&mut self, _input: Name) {}

    fn get_inputs(&self) -> Option<&NameSet> {
        None
    }

    fn run(&self) -> Box<dyn RunningModule> {
        Box::new(Broadcaster {})
    }
}

impl RunningModule for Broadcaster {
    fn receive(&mut self, message: &Message, send: &mut dyn FnMut(Pulse)) {
        send(message.pulse);
    }
}

struct ModuleDescription<T> {
    module: T,
    destinations: Vec<Name>,
}

impl ModuleDescription<Box<dyn Module>> {
    fn parse(table: &mut NameTable, s: &str) -> Result<(Name, Self)> {
        let (kind_and_name, destinations) = s.split_once(" -> ")
            .ok_or("' -> ' expected")?;
        let destinations: Vec<_> =
            destinations.split(", ").map(|name| table.get(name)).collect();
        let (module, name): (Box<dyn Module>, _) =
            if kind_and_name == "broadcaster" {
                (Box::new(Broadcaster {}), table.get("broadcaster"))
            }
            else {
                let mut chars = kind_and_name.chars();
                let kind: Box<dyn Module> = match chars.next() {
                    Some('%') => Box::new(FlipFlop {}),
                    Some('&') => Box::new(Conjunction::new()),
                    _ => Err(format!("Invalid name {kind_and_name}"))?
                };
                (kind, table.get(chars.as_str()))
            };
        Ok((name, Self { module, destinations }))
    }
}

type Configuration = NameMap<ModuleDescription<Box<dyn Module>>>;

type RunningConfiguration = NameMap<ModuleDescription<Box<dyn RunningModule>>>;

fn run(configuration: &Configuration) -> RunningConfiguration {
    configuration.iter().map(|(&name, description)| {
      let module = description.module.run();
      let destinations = description.destinations.clone();
      (name, ModuleDescription { module, destinations })
    }).collect()
}

struct Message {
    input: Name,
    destination: Name,
    pulse: Pulse,
}

trait Observer {
    fn observe(&mut self, message: &Message);
}

struct State<O: Observer> {
    message_queue: std::collections::VecDeque<Message>,
    observer: O,
}

impl<O: Observer> State<O> {
    fn new(observer: O) -> Self {
        Self {
            message_queue: std::collections::VecDeque::new(),
            observer
        }
    }

    fn send_single(&mut self, message: Message) {
        self.observer.observe(&message);
        self.message_queue.push_back(message)
    }

    fn send(&mut self, input: Name, destinations: &Vec<Name>, pulse: Pulse) {
        for &destination in destinations {
            self.send_single(Message { input, destination, pulse });
        }
    }

    fn push_button(&mut self, table: &mut NameTable) {
        let button = table.get("button");
        let broadcaster = table.get("broadcaster");
        self.send_single(
            Message { input: button, destination: broadcaster, pulse: Pulse::Low }
        );
    }

    fn handle_messages(&mut self, configuration: &mut RunningConfiguration) {
        while let Some(message) = self.message_queue.pop_front() {
            if let Some(description) = configuration.get_mut(&message.destination) {
                description.module.receive(&message, &mut |pulse| {
                    self.send(message.destination, &description.destinations, pulse)
                })
            }
        }
    }
}

struct Counter {
    low: u64,
    high: u64,
}

impl Observer for Counter {
    fn observe(&mut self, message: &Message) {
        match message.pulse {
            Pulse::Low => self.low += 1,
            Pulse::High => self.high += 1,
        }
    }
}

fn part1(table: &mut NameTable, configuration: &Configuration) -> u64 {
    let mut running_configuration = run(&configuration);
    let mut state = State::new(Counter { low: 0, high: 0 });
    for _i in 0 .. 1000 {
        state.push_button(table);
        state.handle_messages(&mut running_configuration);
    }
    state.observer.low * state.observer.high
}

struct FindFirstHighPulses<'a> {
    counter: u64,
    inputs: &'a NameSet,
    indices: NameMap<u64>,
}

impl<'a> Observer for FindFirstHighPulses<'a> {
    fn observe(&mut self, message: &Message) {
        if message.pulse == Pulse::High &&
            self.inputs.contains(&message.input) &&
            !self.indices.contains_key(&message.input)
        {
            self.indices.insert(message.input, self.counter);
        }
    }
}

fn part2(
    table: &mut NameTable, configuration: &Configuration, rx_input: Name
) -> Result<u64> {
    let inputs = configuration.get(&rx_input)
        .ok_or("rx input unconfigured")?
        .module.get_inputs().ok_or("rx input is not conjunction")?;
    let mut running_configuration = run(&configuration);
    let mut state = State::new(FindFirstHighPulses {
            counter: 0, inputs: inputs, indices: NameMap::new()
        });
    while !state.observer.inputs.iter().all(|input|
        state.observer.indices.contains_key(input)
    ) {
        state.push_button(table);
        state.observer.counter += 1;
        state.handle_messages(&mut running_configuration);
    }
    Ok(state.observer.indices.values().cloned().fold(1, lcm))
}

fn main() -> Result<()> {
    let mut table = NameTable::new();
    let mut configuration: Configuration = std::io::stdin().lines()
        .map(|line| -> Result<_> {
            ModuleDescription::parse(&mut table, &line?)
        }).collect::<Result<_>>()?;
    let destinations: Vec<(Name, Vec<Name>)> =
        configuration.iter().map(|(&input, description)|
          (input, description.destinations.clone())).collect();
    let rx = table.get("rx");
    let mut rx_input = None;
    for (input, destinations) in destinations {
        for destination in destinations {
            if let Some(description) = configuration.get_mut(&destination) {
                description.module.add_input(input)
            }
            if destination == rx {
                if rx_input.is_some() {
                    Err("Multiple inputs for rx")?
                }
                rx_input = Some(input)
            }
        }
    }
    let result_part1 = part1(&mut table, &configuration);
    println!("Part 1: {result_part1}");
    let rx_input = rx_input.ok_or("no input for rx")?;
    let result_part2 = part2(&mut table, &configuration, rx_input)?;
    println!("Part 2: {result_part2}");
    Ok(())
}
