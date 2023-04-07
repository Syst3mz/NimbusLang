use std::collections::HashSet;
use crate::lr_ast::Terminal::Identifier;
use crate::lr_unary_remover::{Decl, NonTerminal};
use crate::lr_unary_remover::NonTerminal::{Binary, Term};

fn find_dangling_ids(decls: &Vec<Decl>) -> Vec<String> {
    let mut ids_on_right:HashSet<String> = HashSet::new();
    for decl in decls {
        for term in find_ids(&decl.maps_to) {
            ids_on_right.insert(term);
        }
    }

    let mut ids_unused: Vec<String> = vec![];
    for decl in decls {
        if !ids_on_right.contains(&decl.identifier) {
            ids_unused.push(decl.identifier.clone());
        }
    }

    ids_unused
}

fn find_ids(nt: &NonTerminal) -> Vec<String> {
    let mut strings: Vec<String> = vec![];
    match nt {
        Binary { lhs, rhs, bop: _bop } => {
            strings.append(&mut find_ids(lhs));
            strings.append(&mut find_ids(rhs));
        }
        Term(t) => {
            if let Identifier(id) = t {
                strings.push(id.clone());
            }
        }
    }

    strings
}

pub(crate) fn augment_grammar_with_starting_production(decls: &mut Vec<Decl>) {
    let dangling = find_dangling_ids(decls);
    for dangle in dangling {
        decls.push( Decl {
            identifier: "II_INITIAL_PRODUCTION_II".to_string(),
            maps_to: Term(Identifier(dangle)),
        })
    }
}

pub mod test {
    use crate::lr_parser::LRParser;
    use crate::lr_starting_production_adder::find_dangling_ids;
    use crate::lr_unary_remover;
    use crate::lr_unary_remover::{Decl, remove_unary};

    fn build_testing_data(input: &str) -> Vec<Decl> {
        let parsed = LRParser::new(input).parse().unwrap();
        return remove_unary(parsed);
    }

    #[test]
    pub fn find_dangling_ids_check() {
        let u_free = build_testing_data(lr_unary_remover::tests::WRONG_SELF_G);
        assert_eq!(vec!["decl"], find_dangling_ids(&u_free));
    }
}