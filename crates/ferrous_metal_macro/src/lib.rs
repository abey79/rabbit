extern crate proc_macro;
use proc_macro::{Delimiter, Group, Literal, Punct, Spacing, TokenStream, TokenTree};
use std::collections::{HashMap, VecDeque};

fn extract_label(
    mut input: VecDeque<TokenTree>,
    cur_line: usize,
    labels: &mut HashMap<String, usize>,
) -> VecDeque<TokenTree> {
    let mut output = VecDeque::new();
    while !input.is_empty() {
        let tt = input.pop_front().unwrap();
        let next = input.front();

        match (&tt, next) {
            (TokenTree::Ident(ident), Some(TokenTree::Punct(punct))) if punct.as_char() == ':' => {
                labels.insert(ident.to_string(), cur_line);

                // remove ':' from input
                input.pop_front();
            }
            _ => output.push_back(tt),
        }
    }

    output
}

fn rewrite_label(
    mut input: VecDeque<TokenTree>,
    cur_line: usize,
    labels: &HashMap<String, usize>,
) -> Vec<TokenTree> {
    let mut output = Vec::new();

    while !input.is_empty() {
        let tt = input.pop_front().unwrap();
        let next = input.front();

        match (&tt, next) {
            (TokenTree::Ident(ident), _) if labels.contains_key(&ident.to_string()) => {
                let line_num = labels.get(&ident.to_string()).unwrap();
                output.push(TokenTree::Literal(Literal::usize_unsuffixed(*line_num)));
            }
            (TokenTree::Punct(punct), Some(TokenTree::Ident(ident)))
                if punct.as_char() == '#' && labels.contains_key(&ident.to_string()) =>
            {
                let line_num = labels.get(&ident.to_string()).unwrap();
                output.push(TokenTree::Literal(Literal::usize_unsuffixed(
                    line_num - cur_line - 1,
                )));

                // remove identifier
                input.pop_front();
            }
            (TokenTree::Group(group), _) => {
                let delimiter = group.delimiter();
                let stream: VecDeque<_> = group.stream().into_iter().collect();
                let rewritten = rewrite_label(stream, cur_line, labels);
                output.push(TokenTree::Group(Group::new(
                    delimiter,
                    rewritten.into_iter().collect(),
                )));
            }
            _ => output.push(tt),
        }
    }

    output
}

#[proc_macro]
pub fn prog(item: TokenStream) -> TokenStream {
    let input = item.into_iter().collect::<Vec<_>>();

    // first pass: split into lines and extract labels
    let mut labels = HashMap::new();
    let lines = input
        .split(|tt| match tt {
            TokenTree::Punct(punct) => punct.as_char() == ',',
            _ => false,
        })
        .enumerate()
        .map(|(cur_line, tokens)| {
            extract_label(
                VecDeque::from_iter(tokens.iter().cloned()),
                cur_line,
                &mut labels,
            )
        })
        .collect::<Vec<_>>();

    // second pass: replace labels with line numbers
    let lines = lines
        .into_iter()
        .enumerate()
        .map(|(cur_line, tokens)| rewrite_label(tokens, cur_line, &labels));

    // merge lines with commas
    let mut output_stream = TokenStream::new();
    for line in lines {
        if line.is_empty() {
            continue;
        }
        output_stream.extend(line.into_iter());
        output_stream.extend([TokenTree::Punct(Punct::new(',', Spacing::Alone))]);
    }
    TokenTree::Group(Group::new(Delimiter::Bracket, output_stream)).into()
}
