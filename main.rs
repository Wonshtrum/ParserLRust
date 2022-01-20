use std::collections::HashMap;
use std::collections::HashSet;


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
	rule_id: usize,
	lexeme_id: usize,
	lexeme_names: Vec<String>
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


impl Rule {
	fn start(&self, lookahead: Lexeme) -> Position {
		Position {rule: &self, lookahead: lookahead, position: 0}
	}
}


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
}


impl ParserContext {
	fn new() -> ParserContext {
		ParserContext {rule_id: 0, lexeme_id: 0, lexeme_names: Vec::new()}
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
	fn rule(&mut self, product: Lexeme, tokens: Vec<Lexeme>) -> Rule {
		self.rule_id += 1;
		Rule {id: self.rule_id, product: product, tokens: tokens}
	}
}


type First = HashMap<Lexeme, HashSet<Lexeme>>;
fn print_first(first: &First, ctx: &ParserContext) {
	for (key, tokens) in first {
		print!("{}: [ ", key.fmt(ctx));
		for token in tokens {
			print!("{} ", token.fmt(ctx));
		}
		println!("]");
	}
}

fn gen_first(rules: &Vec<Rule>, ctx: &ParserContext) {
	let mut first: First = HashMap::new();
	for rule in rules {
		//println!("{} -> {}", rule.product.fmt(ctx), rule.tokens[0].fmt(ctx));
		if let Some(set) = first.get_mut(&rule.product) {
			set.insert(rule.tokens[0]);
		} else {
			first.insert(rule.product, HashSet::from([rule.tokens[0]]));
		}
	}
	print_first(&first, &ctx);
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
		println!("");
		print_first(&first, &ctx);
	}
}


#[allow(non_snake_case)]
fn main() {
	let mut ctx = ParserContext::new();
	let ACCEPT = ctx.nterm("S'");
	let S = ctx.nterm("S");
	let X = ctx.nterm("X");
	let a = ctx.term("a");
	let b = ctx.term("b");

	let rules = vec![
		ctx.rule(ACCEPT, vec![S]),
		ctx.rule(S, vec![X, X]),
		ctx.rule(X, vec![a, X]),
		ctx.rule(X, vec![b])
	];

	let r = &rules[1];
	println!("{}", r.fmt(&ctx));
	println!("{}", r.start(a).fmt(&ctx));
	let mut p = r.start(b);
	p.position = 1;
	println!("{}", p.fmt(&ctx));

	gen_first(&rules, &ctx);
}
