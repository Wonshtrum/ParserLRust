use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};


#[allow(dead_code)]
enum Color {
	Black = 0,
	Red = 9,
	Green = 10,
	Yellow = 11,
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


struct ParserContext {
	lexeme_id: usize,
	lexeme_names: Vec<String>,
	rule_id: usize,
	rules: Rules
}


struct Rule {
	id: usize,
	product: Lexeme,
	tokens: Vec<Lexeme>
}


struct Position<'a> {
	rule: &'a Rule,
	lookahead: Lexeme,
	position: usize
}


#[derive(Copy, Clone)]
enum ActionType {
	Shift,
	Reduce
}


enum Action<'a> {
	Shift(Position<'a>),
	Reduce(usize)
}


#[derive(Copy, Clone)]
struct Node {
	action: ActionType,
	value: usize
}


type LexSet = HashSet<Lexeme>;
type First = HashMap<Lexeme, LexSet>;
type State<'a> = HashSet<Position<'a>>;
type Rules = Vec<Rule>;


const ACCEPT: Lexeme = Lexeme {id: 1, terminal: false};
const EOF: Lexeme = Lexeme {id: 2, terminal: true};


impl Lexeme {
	fn fmt(&self, ctx: &ParserContext) -> String {
		let name = &ctx.lexeme_names[self.id-1];
		if self.terminal {
			name.clone()
		} else {
			colored(name, Color::Red, true)
		}
	}
}


impl Rule {
	fn fmt(&self, ctx: &ParserContext) -> String {
		let mut tokens_repr = String::new();
		for token in &self.tokens {
			tokens_repr.push_str(" ");
			tokens_repr.push_str(&token.fmt(ctx));
		}
		format!("{}. {} -> {}", self.id, self.product.fmt(ctx), tokens_repr)
	}
	fn start(&self, lookahead: Lexeme) -> Position {
		Position {rule: &self, lookahead: lookahead, position: 0}
	}
}


impl Position<'_> {
	fn fmt(&self, ctx: &ParserContext) -> String {
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
	fn expand<'a>(&self, ctx: &'a ParserContext, first: &First) -> State<'a> {
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
fn next_action<'a>(entry: &Position, ctx: &'a ParserContext) -> (Lexeme, Action<'a>) {
	if let Some(at) = entry.at(0) {
		(at, Action::Shift(Position {rule: &ctx.rules[entry.rule.id-1], lookahead: entry.lookahead, position: entry.position+1}))
	} else {
		(entry.lookahead, Action::Reduce(entry.rule.id))
	}
}


impl Hash for Position<'_> {
	fn hash<H: Hasher>(&self, state: &mut H) {
		self.rule.id.hash(state);
		self.position.hash(state);
		self.lookahead.hash(state);
	}
}
impl PartialEq for Position<'_> {
	fn eq(&self, other: &Position) -> bool {
		self.rule.id == other.rule.id &&
		self.position == other.position &&
		self.lookahead == other.lookahead
	}
}
impl Eq for Position<'_> {}


impl ParserContext {
	fn new() -> ParserContext {
		let mut ctx = ParserContext {rule_id: 0, lexeme_id: 0, lexeme_names: Vec::new(), rules: Vec::new()};
		ctx.nterm("S'");
		ctx.term("$");
		ctx
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
	fn rule(&mut self, product: Lexeme, tokens: Vec<Lexeme>) {
		if self.rule_id == 0 {
			assert!(product == ACCEPT, "First rule must produce ACCEPT");
		}
		self.rule_id += 1;
		let rule = Rule {id: self.rule_id, product: product, tokens: tokens};
		self.rules.push(rule);
	}
}


fn fmt_lexset(tokens: &LexSet, ctx: &ParserContext) -> String {
	let mut result = String::from("[ ");
	for token in tokens {
		result.push_str(&token.fmt(ctx));
		result.push_str(" ");
	}
	result.push_str("]");
	result
}
fn fmt_first(first: &First, ctx: &ParserContext) -> String {
	let mut result = String::new();
	for (key, tokens) in first {
		result.push_str(&format!("{}: {}\n", key.fmt(ctx), fmt_lexset(tokens, ctx)));
	}
	result
}
fn fmt_state(state: &State, ctx: &ParserContext) -> String {
	let mut result = String::new();
	for entry in state {
		result.push_str(&entry.fmt(ctx));
		result.push_str("\n");
	}
	result
}


fn gen_first(rules: &Rules, ctx: &ParserContext) -> First {
	let mut first: First = HashMap::new();
	for rule in rules {
		//println!("{} -> {}", rule.product.fmt(ctx), rule.tokens[0].fmt(ctx));
		if let Some(set) = first.get_mut(&rule.product) {
			set.insert(rule.tokens[0]);
		} else {
			first.insert(rule.product, HashSet::from([rule.tokens[0]]));
		}
	}
	println!("{}", fmt_first(&first, &ctx));
	let mut changed = true;
	while changed {
		changed = false;
		let mut new_first: First = HashMap::new();
		for (key, tokens) in &first {
			let mut new_tokens = HashSet::new();
			for token in tokens {
				if token.terminal {
					new_tokens.insert(*token);
				} else if let Some(set) = first.get(token) {
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
		first = new_first;
		println!("{}", fmt_first(&first, &ctx));
	}
	first
}


fn expand_state<'a>(state:State<'a>, ctx: &'a ParserContext, first: &First) -> State<'a> {
	let mut waiting = state;
	let mut new_state: State = HashSet::new();
	while !waiting.is_empty() {
		let mut new_waiting: State = HashSet::new();
		for entry in waiting {
			if !new_state.contains(&entry) {
				for sub_entry in entry.expand(ctx, &first) {
					new_waiting.insert(sub_entry);
				}
				new_state.insert(entry);
			}
		}
		waiting = new_waiting;
	}
	new_state
}


fn build_automaton(ctx: &ParserContext, first: &First) {
	let mut states: HashMap<usize, State> = HashMap::new();
	let mut graph: HashMap<(usize, Lexeme), Node> = HashMap::new();
	let mut next_state_id = 1;
	let mut waiting = vec![(0, HashSet::from([ctx.rules[0].start(EOF)]))];
	while !waiting.is_empty() {
		let mut new_waiting = Vec::new();
		for (state_id, state) in waiting {
			let state = expand_state(state, ctx, first);
			states.insert(state_id, state);
			let state = states.get(&state_id).unwrap();
			/*let mut merge = false;
			if merge {
				continue;
			}*/
			let mut shift: HashMap<Lexeme, State> = HashMap::new();
			let mut reduce: HashMap<Lexeme, usize> = HashMap::new();
			for entry in state {
				match next_action(&entry, ctx) {
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
		if states.len()>1 {
			break;
		}
	}
	for (state_id, state) in states {
		println!("State {}:\n{}", state_id, fmt_state(&state, ctx));
	}
}


#[allow(non_snake_case)]
fn main() {
	let mut ctx = ParserContext::new();
	let S = ctx.nterm("S");
	let X = ctx.nterm("X");
	let a = ctx.term("a");
	let b = ctx.term("b");

	ctx.rule(ACCEPT, vec![S]);
	ctx.rule(S, vec![X, X]);
	ctx.rule(X, vec![a, X]);
	ctx.rule(X, vec![b]);

	let r = &ctx.rules[1];
	println!("{}", r.fmt(&ctx));
	println!("{}", r.start(a).fmt(&ctx));
	let mut p = r.start(b);
	p.position = 1;
	println!("{}", p.fmt(&ctx));

	let first = gen_first(&ctx.rules, &ctx);
	let mut state = HashSet::from([ctx.rules[0].start(EOF)]);
	println!("{}", fmt_state(&state, &ctx));
	state = expand_state(state, &ctx, &first);
	println!("{}", fmt_state(&state, &ctx));

	build_automaton(&ctx, &first);
}
