use colored::*;
use std::collections::HashMap;
pub type Spanned<T> = (T, Span);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span(usize);

impl Span {
    pub fn dummy() -> Span {
        Span { 0: 9999999999 }
    }
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "span: {}", self.0)
    }
}

#[derive(Debug, Default)]
pub struct SpanManager {
    sources: Vec<String>,
    spans: Vec<(usize, usize, usize)>,
}

impl SpanManager {
    pub fn add_source(&mut self, source: String) -> SpanMaker {
        let i = self.sources.len();
        self.sources.push(source);
        SpanMaker {
            parent: self,
            source_ind: i,
            pool: Default::default(),
        }
    }

    fn new_span(&mut self, source_ind: usize, l: usize, r: usize) -> Span {
        let i = self.spans.len();
        self.spans.push((source_ind, l, r));
        Span(i)
    }

    pub fn resolve_span(&self, span: Span) -> (usize, usize, usize) {
        self.spans[span.0]
    }

    pub fn print(&self, span: Span) -> String {
        let (source_ind, mut l, mut r) = self.resolve_span(span);
        let source = &self.sources[source_ind];
        let mut out = String::new();

        if l < source.len() && r < source.len() {
            l += 1;
            r += 1;
        }

        assert!(l <= r);

        let (before, source) = source.split_at(l);

        let tok = source.split_at(r - l).0.split('\n').next().unwrap();
        let after = source.split_at(tok.len()).1;

        let mut b_iter = before.rsplit('\n');
        let line_before = b_iter.next().unwrap();
        let mut a_iter = after.split('\n');
        let line_after = a_iter.next().unwrap();

        if let Some(line) = b_iter.next() {
            if let Some(line) = b_iter.next() {
                out += "| ";
                out += line;
                out += "\n| ";
            }

            if line.trim().len() > 0 {
                out += line;
                out += "\n";
            }
            out += "";
        } else {
            out += "| ";
        }

        // Line of the span
        out += line_before;
        out += tok;
        out += line_after;
        out += "\n|";

        // highlight line
        out += &" ".repeat(line_before.len());

        let mut line = String::from("^");
        if tok.len() > 0 {
            line += &".".repeat(std::cmp::max(1, tok.len() - 1));
        }
        out += &line.red().to_string();

        out += &" ".repeat(line_after.len());
        out += "\n";

        // Lines after the span
        for _ in 0..1 {
            if let Some(line) = a_iter.next() {
                out += "| ";
                out += line;
                out += "\n|";
            } else {
                out += "|";
            }
        }

        out
    }
}

#[derive(Debug)]
pub struct SpanMaker<'a> {
    parent: &'a mut SpanManager,
    source_ind: usize,
    pool: HashMap<(usize, usize), Span>,
}

impl<'a> SpanMaker<'a> {
    pub fn span(&mut self, l: usize, r: usize) -> Span {
        let source_ind = self.source_ind;
        let parent = &mut self.parent;

        *self
            .pool
            .entry((l, r))
            .or_insert_with(|| parent.new_span(source_ind, l, r))
    }
}

#[derive(Debug)]
pub struct SpannedError {
    pairs: Vec<(String, Span)>,
}

impl SpannedError {
    pub fn new1(s1: impl Into<String>, s2: Span) -> Self {
        let p1 = (s1.into(), s2);
        SpannedError { pairs: vec![p1] }
    }

    pub fn new2(s1: impl Into<String>, s2: Span, s3: impl Into<String>, s4: Span) -> Self {
        let p1 = (s1.into(), s2);
        let p2 = (s3.into(), s4);
        SpannedError {
            pairs: vec![p1, p2],
        }
    }

    pub fn print(&self, sm: &SpanManager) -> String {
        let mut out = String::new();
        for (msg, span) in self.pairs.iter() {
            let err_msg = format!("\n[ERROR:1:3]> {}", &msg);
            out += &err_msg;
            out += "\n ";
            out += &"_".repeat(err_msg.len());
            out += "\n| ";
            out += "\n";
            out += &sm.print(*span);
            out += &"_".repeat(err_msg.len());
            out += "\n";
        }
        out
    }
}

// impl std::fmt::Display for SpannedError {
//     fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         Ok(())
//     }
// }

// impl std::error::Error for SpannedError {}
