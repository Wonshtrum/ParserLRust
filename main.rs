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
struct Lexeme {
	id: usize,
	terminal: bool
}


struct ParserContext {
	rule_id: usize,
	lexeme_id: usize,
	lexeme_names: Vec<String>
}


struct Rule<'a> {
	ctx: &'a ParserContext,
	id: usize,
	product: Lexeme,
	tokens: Vec<Lexeme>
}


struct Position<'a> {
	rule: &'a Rule<'a>,
	lookahead: Lexeme,
	position: usize
}


impl Rule<'_> {
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


impl std::fmt::Display for Rule<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		let mut tokens_repr = String::new();
		for token in &self.tokens {
			tokens_repr.push_str(" ");
			tokens_repr.push_str(&token.fmt(&self.ctx));
		}
		write!(f, "{}. {} -> {}", self.id, self.product.fmt(&self.ctx), tokens_repr)
	}
}


impl std::fmt::Display for Position<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		let mut tokens_repr = String::new();
		let dot = colored("∘", Color::Green, true);
		for (i, token) in self.rule.tokens.iter().enumerate() {
			tokens_repr.push_str(if i == self.position {&dot} else {" "});
			tokens_repr.push_str(&token.fmt(&self.rule.ctx));
		}
		write!(f, "{}. {} -> {} , {}", self.rule.id, self.rule.product.fmt(&self.rule.ctx), tokens_repr, self.lookahead.fmt(&self.rule.ctx))
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
		Rule {ctx: self, id: self.rule_id, product: product, tokens: tokens}
	}
}


#[allow(non_snake_case)]
fn main() {
	let mut ctx = ParserContext::new();
	let S = ctx.nterm("S");
	let X = ctx.nterm("X");
	let a = ctx.term("a");
	let b = ctx.term("b");

	let r = ctx.rule(S, vec![X, X]);
	println!("{}", r);
	println!("{}", r.start(a));
	let mut p = r.start(b);
	p.position = 1;
	println!("{}", p);
}
