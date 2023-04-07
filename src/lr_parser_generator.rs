use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use crate::lr_ir::{Decl, Terminal};
use crate::lr_ir::Terminal::Empty;

#[derive(Eq, PartialEq, Hash, Debug, Copy, Clone)]
struct Item {
    rule_index: usize,
    dot_position: usize,
}

pub(crate) fn generate_lr_item_set(decls: Vec<Decl>) {
    let mut dict = dictify(decls.clone());

}

fn is_terminal(t: &Terminal) -> bool {
    match t {
        Terminal::Identifier(_) => false,
        Terminal::StringLiteral(_) => true,
        Terminal::Empty => {true}
    }
}

fn dictify(decls: Vec<Decl>) -> HashMap<String, Vec<Vec<Terminal>>> {
    let mut ret: HashMap<String, Vec<Vec<Terminal>>> = HashMap::new();
    for decl in decls {
        if !ret.contains_key(&decl.identifier) {
            ret.insert(decl.identifier.clone(), vec![]);
        }

        let t = ret.get_mut(&decl.identifier).unwrap();
        t.push(decl.maps_to);
    }

    ret
}


fn predict(mut items: Vec<Item>, decls: &Vec<Decl>) -> HashSet<Item> {
    let predicted_items = items.clone();
    let mut prediction: HashSet<Item> = HashSet::from_iter(predicted_items.iter().map(|x| *x));
    let mut p = prediction.len();
    while let Some(item) = items.pop() {
        let sym = after_dot(item, decls);
        for (index, decl) in decls.iter().enumerate() {
            if let Some(sym) = sym {
                if *sym == Terminal::Identifier(decl.identifier.clone()) {
                    prediction.insert(Item { rule_index: index, dot_position: 0 });
                    if p < prediction.len() {
                        p = prediction.len();
                        items.push(Item { rule_index: index, dot_position: 0 })
                    }
                }
            }
        }
    }

    return prediction;
}
fn find_index_of_initial_production(decls: &Vec<Decl>) -> usize {
    for (index, decl) in decls.iter().enumerate() {
        if decl.identifier == "II_INITIAL_PRODUCTION_II" {
            return index;
        }
    }

    return 0;
}

fn after_dot(item: Item, decl: &Vec<Decl>) -> Option<&Terminal> {
    decl.get(item.rule_index)?.maps_to.get(item.dot_position)
}

fn partition(mut items: HashSet<Item>, decl: &Vec<Decl>) {
    let mut groups: HashMap<Option<&Terminal>,HashSet<Item>> = HashMap::new();
    for mut item in items {
        let sym = after_dot(item, decl);
        if let Some(sym) = sym {
            item.dot_position += 1;
        }

        if !groups.contains_key(&sym) {
            groups.insert(sym, HashSet::new());
        }

        groups.get
    }
    return groups.item
}

fn print_grammar(decls: &Vec<Decl>) {
    for decl in decls {
        println!("{} -> {}", decl.identifier, join(" ", &decl.maps_to[..]))
    }
}

fn print_item<'a>(prefix: &str, item: Item, decls: &Vec<Decl>) {
    println!("{prefix}{} -> {} ∘ {}",
             decls[item.rule_index].identifier,
             join(" ", &decls[item.rule_index].maps_to[..item.dot_position]),
             join(" ", &decls[item.rule_index].maps_to[item.dot_position..])
    )
}

fn print_itemset(index: usize, items: HashSet<Item>, decls: &Vec<Decl>) {
    let mut prefix = format!("{}: ", index);
    for item in items {
        print_item(&prefix, item, decls)
    }
}


fn join<T: ToString>(separator:&str, maps_to: &[T]) -> String {
    maps_to.iter().map(|x| x.to_string()).collect::<Vec<String>>().join(separator)
}



#[cfg(test)]
pub mod test {
    use std::collections::HashMap;
    use regex::Regex;
    use crate::lr_ir::{Decl, to_ir};
    use crate::lr_ir::Terminal::Identifier;
    use crate::lr_or_remover::remove_or;
    use crate::lr_parser::LRParser;
    use crate::lr_parser_generator::{dictify, Item, predict, print_itemset};
    use crate::lr_starting_production_adder::augment_grammar_with_starting_production;
    use crate::lr_unary_remover::remove_unary;

    const PD_G:&str = r"program -> program* declaration
    declaration -> varDecl | constDecl | statement";

    fn build_testing_data(input: &str) -> Vec<Decl>{
        let parsed = LRParser::new(input).parse().unwrap();
        let mut parsed = remove_unary(parsed);
        augment_grammar_with_starting_production(&mut parsed);
        let parsed = remove_or(parsed);
        let parsed = to_ir(parsed);
        return parsed;
    }

    // test from http://epgp.inflibnet.ac.in/epgpdata/uploads/epgp_content/S000007CS/P001069/M017964/ET/1480497459Module14_Content_final.pdf
    #[test]
    fn pd_check() {
        let ir = build_testing_data(PD_G);
        print_itemset(0, predict(vec![Item { rule_index: 0, dot_position: 0 }], &ir), &ir);
        assert!(false)
    }
}