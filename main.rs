use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::{Hash, Hasher};
use std::iter::FromIterator;


#[allow(dead_code)]
enum Color {
	Black = 0,
	Red = 9,
	Green = 10,
	Yellow = 11,
	White = 15,
	Blue = 21,
}


fn colored(text: &str, color: Color, bold: bool) -> String {
	format!("\x1b[{};38;5;{}m{}\x1b[0m", if bold {1} else {0}, color as u8, text)
}


#[derive(Copy, Clone)]
#[derive(PartialEq, Eq, Hash)]
struct Lexeme {
	id: usize,
	terminal: bool
}


struct Token<T> {
	kind: Lexeme,
	value: TokenValue<T>
}


struct ParserContext<T> {
	lexeme_id: usize,
	lexeme_names: Vec<String>,
	rule_id: usize,
	rules: Rules<T>,
	first: First,
	graph: Graph
}


struct Rule<T> {
	id: usize,
	product: Lexeme,
	tokens: Vec<Lexeme>,
	method: ReduceMethod<T>
}


struct Position<'a, T> {
	rule: &'a Rule<T>,
	lookahead: Lexeme,
	position: usize
}


#[derive(PartialEq)]
#[derive(Copy, Clone)]
enum ActionType {
	Shift,
	Reduce
}


enum Action<'a, T> {
	Shift(Position<'a, T>),
	Reduce(usize)
}


#[derive(Copy, Clone)]
struct Node {
	action: ActionType,
	value: usize
}


type LexSet = HashSet<Lexeme>;
type First = HashMap<Lexeme, LexSet>;
type State<'a, T> = HashSet<Position<'a, T>>;
type States<'a, T> = HashMap<usize, State<'a, T>>;
type Rules<T> = Vec<Rule<T>>;
type Graph = HashMap<(usize, Lexeme), Node>;
type TokenValue<T> = Option<T>;
type ReduceMethod<T> = Box<dyn Fn(Vec<TokenValue<T>>) -> TokenValue<T>>;


const ACCEPT: Lexeme = Lexeme {id: 1, terminal: false};
const EOF: Lexeme = Lexeme {id: 2, terminal: true};


impl Lexeme {
	fn fmt<T>(&self, ctx: &ParserContext<T>) -> String {
		let name = &ctx.lexeme_names[self.id-1];
		if self.terminal {
			name.clone()
		} else {
			colored(name, Color::Red, true)
		}
	}
}


impl<T> Rule<T> {
	fn fmt(&self, ctx: &ParserContext<T>) -> String {
		let mut tokens_repr = String::new();
		for token in &self.tokens {
			tokens_repr.push_str(" ");
			tokens_repr.push_str(&token.fmt(ctx));
		}
		format!("{}. {} -> {}", self.id, self.product.fmt(ctx), tokens_repr)
	}
	fn start(&self, lookahead: Lexeme) -> Position<T> {
		Position {rule: &self, lookahead: lookahead, position: 0}
	}
}


impl<T> Position<'_, T> {
	fn fmt(&self, ctx: &ParserContext<T>) -> String {
		let mut tokens_repr = String::new();
		let dot = colored("∘", Color::Green, true);
		for (i, token) in self.rule.tokens.iter().enumerate() {
			tokens_repr.push_str(if i == self.position {&dot} else {" "});
			tokens_repr.push_str(&token.fmt(ctx));
		}
		format!("{}. {} -> {} , {}", self.rule.id, self.rule.product.fmt(ctx), tokens_repr, self.lookahead.fmt(ctx))
	}
	fn at(&self, add: usize) -> Option<Lexeme> {
		let position = self.position+add;
		if position >= self.rule.tokens.len() {
			None
		} else {
			Some(self.rule.tokens[position])
		}
	}
	fn next_lookahead(&self, first: &First) -> LexSet {
		if let Some(at) = self.at(1) {
			if at.terminal {
				HashSet::from([at])
			} else if let Some(set) = first.get(&at) {
				set.clone()
			} else {
				HashSet::new()
			}
		} else {
			HashSet::from([self.lookahead])
		}
	}
	fn expand<'a>(&self, ctx: &'a ParserContext<T>, first: &First) -> State<'a, T> {
		let mut result = HashSet::new();
		if let Some(at) = self.at(0) {
			if !at.terminal {
				for rule in &ctx.rules {
					if rule.product == at {
						for lookahead in self.next_lookahead(first) {
							result.insert(rule.start(lookahead));
						}
					}
				}
			}
		}
		result
	}
}
fn next_action<'a, T>(entry: &Position<T>, ctx: &'a ParserContext<T>) -> (Lexeme, Action<'a, T>) {
	if let Some(at) = entry.at(0) {
		(at, Action::Shift(Position {rule: &ctx.rules[entry.rule.id-1], lookahead: entry.lookahead, position: entry.position+1}))
	} else {
		(entry.lookahead, Action::Reduce(entry.rule.id))
	}
}


impl<T> Hash for Position<'_, T> {
	fn hash<H: Hasher>(&self, state: &mut H) {
		self.rule.id.hash(state);
		self.position.hash(state);
		self.lookahead.hash(state);
	}
}
impl<T> PartialEq for Position<'_, T> {
	fn eq(&self, other: &Position<T>) -> bool {
		self.rule.id == other.rule.id &&
		self.position == other.position &&
		self.lookahead == other.lookahead
	}
}
impl<T> Eq for Position<'_, T> {}


impl<T> ParserContext<T> {
	fn new() -> ParserContext<T> {
		let mut ctx = ParserContext {
			rule_id: 0,
			lexeme_id: 0,
			lexeme_names: Vec::new(),
			rules: Vec::new(),
			first: HashMap::new(),
			graph: HashMap::new()
		};
		ctx.nterm(&colored("S'", Color::Blue, true));
		ctx.term(&colored("$", Color::Blue, true));
		ctx
	}
	fn fmt(&self) -> String {
		let mut result = String::new();
		for rule in &self.rules {
			result.push_str(&rule.fmt(self));
			result.push_str("\n");
		}
		result
	}
	fn term(&mut self, name: &str) -> Lexeme {
		self.lexeme_id += 1;
		self.lexeme_names.push(String::from(name));
		Lexeme {id: self.lexeme_id, terminal: true}
	}
	fn nterm(&mut self, name: &str) -> Lexeme {
		self.lexeme_id += 1;
		self.lexeme_names.push(String::from(name));
		Lexeme {id: self.lexeme_id, terminal: false}
	}
	fn rule(&mut self, product: Lexeme, tokens: Vec<Lexeme>, method: ReduceMethod<T>) {
		if self.rule_id == 0 {
			assert!(product == ACCEPT, "First rule must produce ACCEPT");
		}
		self.rule_id += 1;
		let rule = Rule {id: self.rule_id, product, tokens, method};
		self.rules.push(rule);
	}
	fn gen_first(&mut self) {
		for rule in &self.rules {
			//println!("{} -> {}", rule.product.fmt(ctx), rule.tokens[0].fmt(ctx));
			if let Some(set) = self.first.get_mut(&rule.product) {
				set.insert(rule.tokens[0]);
			} else {
				self.first.insert(rule.product, HashSet::from([rule.tokens[0]]));
			}
		}
		println!("{}", fmt_first(&self.first, &self));
		let mut changed = true;
		while changed {
			changed = false;
			let mut new_first: First = HashMap::new();
			for (key, tokens) in &self.first {
				let mut new_tokens = HashSet::new();
				for token in tokens {
					if token.terminal {
						new_tokens.insert(*token);
					} else if let Some(set) = self.first.get(token) {
						for t in set {
							if !t.terminal {
								changed = true;
							}
							new_tokens.insert(*t);
						}
					}
				}
				new_first.insert(*key, new_tokens);
			}
			self.first = new_first;
			println!("{}", fmt_first(&self.first, &self));
		}
	}
	fn build_automaton(&mut self) {
		self.gen_first();
		let mut graph: Graph = HashMap::new();
		let mut states: States<T> = HashMap::new();
		let mut next_state_id = 1;
		let mut waiting = vec![(0, HashSet::from([self.rules[0].start(EOF)]))];
		while !waiting.is_empty() {
			let mut new_waiting = Vec::new();
			for (state_id, state) in waiting {
				let state = expand_state(state, self);
				let mut merge = Vec::new();
				for (other_id, other) in &states {
					if other == &state {
						for (k, v) in &graph {
							if v.action == ActionType::Shift && v.value == state_id {
								merge.push((*k, *other_id));
							}
						}
					}
				}
				if !merge.is_empty() {
					for (k, v) in merge {
						graph.insert(k, Node {action: ActionType::Shift, value: v});
					}
					continue;
				}
				states.insert(state_id, state);
				let state = states.get(&state_id).unwrap();
				let mut shift: HashMap<Lexeme, State<T>> = HashMap::new();
				let mut reduce: HashMap<Lexeme, usize> = HashMap::new();
				for entry in state {
					match next_action(&entry, self) {
						(token, Action::Shift(new_entry)) => {
							if reduce.contains_key(&token) {
								panic!("Shift/Reduce conflict");
							}
							if let Some(set) = shift.get_mut(&token) {
								set.insert(new_entry);
							} else {
								shift.insert(token, HashSet::from([new_entry]));
							}
						}
						(token, Action::Reduce(rule_id)) => {
							if reduce.contains_key(&token) {
								panic!("Reduce/Reduce conflict");
							}
							if shift.contains_key(&token) {
								panic!("Shift/Reduce conflict");
							}
							reduce.insert(token, rule_id);
						}
					}
				}
				for (k, v) in shift {
					new_waiting.push((next_state_id, v));
					graph.insert((state_id, k), Node {action: ActionType::Shift, value: next_state_id});
					next_state_id += 1;
				}
				for (k, v) in reduce {
					graph.insert((state_id, k), Node {action: ActionType::Reduce, value: v});
				}
			}
			waiting = new_waiting;
		}
		println!("{}", fmt_states(&states, self));
		self.graph = graph;
	}
	fn parse(&self, mut stream: VecDeque<Token<T>>) -> bool {
		stream.push_back(Token {kind: EOF, value: None});
		let mut state_stack = vec![0];
		//let build_stack: Vec<TokenValue<T>> = Vec::new();
		while !stream.is_empty() {
			let state = state_stack[state_stack.len()-1];
			let token = stream.front().unwrap();
			let key = (state, token.kind);
			if let Some(node) = self.graph.get(&key) {
				println!("({}, {}) : {}", state, token.kind.fmt(self), fmt_node(node));
				match node.action {
					ActionType::Reduce => {
						let reduction = &self.rules[node.value-1];
						if reduction.product == ACCEPT {
							println!("{}", colored("Valid input", Color::Green, true));
							return true;
						}
						//(reduction.method)(build_stack);
						state_stack.truncate(state_stack.len().saturating_sub(reduction.tokens.len()));
						stream.push_front(Token {kind:reduction.product, value: None });
					}
					ActionType::Shift => {
						state_stack.push(node.value);
						stream.pop_front();
					}
				}
			} else {
				let expected = self.graph.keys().filter(|(s, t)| s == &state && t.terminal).map(|(_, t)| *t).collect::<Vec<Lexeme>>();
				println!("Found: {}\nExpected: {}", token.kind.fmt(self), fmt_lexset(&HashSet::from_iter(expected), self));
				return false;
			}
		}
		unreachable!();
	}
}



fn fmt_lexset<T>(tokens: &LexSet, ctx: &ParserContext<T>) -> String {
	let mut result = String::from("[ ");
	for token in tokens {
		result.push_str(&token.fmt(ctx));
		result.push_str(" ");
	}
	result.push_str("]");
	result
}
fn fmt_first<T>(first: &First, ctx: &ParserContext<T>) -> String {
	let mut result = String::new();
	for (key, tokens) in first {
		result.push_str(&format!("{}: {}\n", key.fmt(ctx), fmt_lexset(tokens, ctx)));
	}
	result
}
fn fmt_state<T>(state: &State<T>, ctx: &ParserContext<T>) -> String {
	let mut result = String::new();
	for entry in state {
		result.push_str(&entry.fmt(ctx));
		result.push_str("\n");
	}
	result
}
fn fmt_states<T>(states: &States<T>, ctx: &ParserContext<T>) -> String {
	let mut result = String::new();
	for (state_id, state) in states {
		result.push_str(&format!("State {}:\n{}", state_id, fmt_state(&state, &ctx)));
	}
	result
}
fn fmt_node(node: &Node) -> String {
	match node.action {
		ActionType::Shift => colored(&format!("S{}", node.value), Color::White, false),
		ActionType::Reduce => colored(&format!("R{}", node.value), Color::Yellow, true)
	}
}
fn fmt_graph<T>(graph: &Graph, ctx: &ParserContext<T>) -> String {
	let mut result = String::new();
	for ((state_id, token), node) in graph {
		result.push_str(&format!("({}, {}) -> {}\n",
			state_id,
			token.fmt(&ctx),
			fmt_node(node)));
	}
	result
}


fn expand_state<'a, T>(state:State<'a, T>, ctx: &'a ParserContext<T>) -> State<'a, T> {
	let mut waiting = state;
	let mut new_state: State<T> = HashSet::new();
	while !waiting.is_empty() {
		let mut new_waiting: State<T> = HashSet::new();
		for entry in waiting {
			if !new_state.contains(&entry) {
				for sub_entry in entry.expand(ctx, &ctx.first) {
					new_waiting.insert(sub_entry);
				}
				new_state.insert(entry);
			}
		}
		waiting = new_waiting;
	}
	new_state
}


macro_rules! RULE {
	($T:ident [$v:ident $i:ident] $prod:ident $func:block) => {
		{
			Some($T::$prod($func))
		}
	};
	($T:ident $l:ident($n:ident : $t:ident) $($l_:ident($n_:ident : $t_:ident))* [$v:ident $i:ident] $prod:ident $func:block) => {
		{
			println!("here!!!");
			$i += 1;
			if let Some($T::$t($n)) = &$v[$i-1] {
				RULE!($T $($l_($n_:$t_))* [$v $i] $prod $func)
			} else {
				unreachable!();
			}
		}
	};
	(in $ctx:ident<$T:ident> where $product:ident : $($l:ident($n:ident : $t:ident))* => $prod:ident $func:block) => {
		{
			$ctx.rule($product, vec![$($l,)*], Box::from(|v:Vec<TokenValue<$T>>| {
				let mut i = 0;
				RULE!($T $($l($n:$t))* [v i] $prod $func)
			}));
		}
	};
}


#[allow(dead_code)]
enum TokenResults {
	Number(usize),
}


#[allow(non_snake_case)]
fn main() {
	let mut ctx = ParserContext::new();
	let S = ctx.nterm("S");
	let X = ctx.nterm("X");
	let a = ctx.term("a");
	let b = ctx.term("b");

	RULE!(in ctx<TokenResults> where ACCEPT: S(s:Number) => Number {
		*s
	});
	RULE!(in ctx<TokenResults> where S: X(a:Number) X(b:Number) => Number {
		a+b+1
	});
	RULE!(in ctx<TokenResults> where X: a(a:Number) X(b:Number) => Number {
		a+b+1
	});
	RULE!(in ctx<TokenResults> where X: b(b:Number) => Number {
		b+1
	});

	println!("{}", ctx.fmt());
	ctx.build_automaton();

	println!("{}", fmt_graph(&ctx.graph, &ctx));

	let t_a = Token {kind: b, value: None};
	let t_b = Token {kind: b, value: None};
	ctx.parse(VecDeque::from([t_a, t_b]));
}
