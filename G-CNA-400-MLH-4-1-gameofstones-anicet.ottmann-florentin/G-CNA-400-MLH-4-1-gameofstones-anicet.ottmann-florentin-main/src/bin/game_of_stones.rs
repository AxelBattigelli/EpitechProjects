use std::{
    fs::File,
    io::BufReader,
    process::{ExitCode, Termination},
};

use game_of_stones::PeopleTree;

fn links<A: IntoIterator<Item = String>>(args: A) -> Result<(), ()> {
    let mut args = args.into_iter();
    let Some(fr) = args.next() else {
        return Err(());
    };
    let Some(p1) = args.next() else {
        return Err(());
    };
    let Some(p2) = args.next() else {
        return Err(());
    };

    if args.next().is_some() {
        return Err(());
    }

    let data = game_of_stones::load_data(
        BufReader::new(File::open(fr).map_err(|_| ())?),
        " is friends with ",
    )?;
    if data.is_empty() {
        return Err(());
    }

    let tree = PeopleTree::new(data)?;

    println!(
        "Degree of separation between {} and {}: {:?}",
        p1,
        p2,
        tree.separation(&p1, &p2).map_or(-1, i64::from)
    );
    Ok(())
}

fn plots<A: IntoIterator<Item = String>>(args: A) -> Result<(), ()> {
    let mut args = args.into_iter();
    let Some(fr) = args.next() else {
        return Err(());
    };
    let Some(cr) = args.next() else {
        return Err(());
    };
    let Some(n_str) = args.next() else {
        return Err(());
    };
    let n: u32 = n_str.parse().map_err(|_| ())?;

    if args.next().is_some() {
        return Err(());
    }

    let data = game_of_stones::load_data(
        BufReader::new(File::open(fr).map_err(|_| ())?),
        " is friends with ",
    )?;
    if data.is_empty() {
        return Err(());
    }

    let mut tree = PeopleTree::new(data)?;

    let data = game_of_stones::load_data(
        BufReader::new(File::open(cr).map_err(|_| ())?),
        " is plotting against ",
    )?;
    tree.add_plots(data)?;

    println!("Names:");
    let sorted_names = tree.all_names_sorted();
    for name in &sorted_names {
        println!("{name}");
    }

    println!("\nRelationships:");
    for i_name in &sorted_names {
        for (j, j_name) in sorted_names.iter().enumerate() {
            let nb_rel = tree.separation(i_name, j_name);

            if j != 0 {
                print!(" ");
            }
            nb_rel.map_or_else(
                || {
                    print!("0");
                },
                |dist| {
                    if dist <= n {
                        print!("{dist:?}");
                    } else {
                        print!("0");
                    }
                },
            );
        }
        println!();
    }

    plots_next(n, &tree)
}

fn compare_allies(tree: &PeopleTree, left: &str, right: &str) -> std::cmp::Ordering {
    match (
        tree.is_plotting(left, "Cersei Lannister"),
        tree.is_plotting(right, "Cersei Lannister"),
    ) {
        (true, false) => return std::cmp::Ordering::Less,
        (false, true) => return std::cmp::Ordering::Greater,
        _ => (),
    }
    match tree
        .separation(left, "Cersei Lannister")
        .cmp(&tree.separation(right, "Cersei Lannister"))
    {
        std::cmp::Ordering::Equal => (),
        value => return value,
    }
    left.cmp(right)
}

fn try_make_far_friend_ally(
    chains: &Vec<Vec<(String, String)>>,
    n: u32,
    tree: &PeopleTree,
    potential_queen_ally: &str,
) -> Result<Option<Vec<(String, String)>>, ()> {
    for ally_ennemy in tree.get_ennemies(potential_queen_ally).ok_or(())? {
        if let Some(potential_allies) = tree.get_ennemies(ally_ennemy) {
            let mut potential_allies = potential_allies.collect::<Vec<_>>();
            potential_allies.sort_unstable_by(|l, r| compare_allies(tree, l, r));
            for potential_ally in potential_allies {
                if chains.iter().flatten().any(|(v, _)| v == potential_ally) {
                    continue;
                }
                match tree.separation("Cersei Lannister", potential_ally) {
                    Some(sep) if sep <= n => {
                        // Close Friend
                        return Ok(Some(vec![(
                            potential_ally.to_owned(),
                            ally_ennemy.to_owned(),
                        )]));
                    }
                    Some(_) => {
                        // Far Friend
                        match try_make_far_friend_ally(
                            &[
                                chains.to_owned(),
                                vec![vec![(potential_ally.to_owned(), ally_ennemy.to_owned())]],
                            ]
                            .concat(),
                            n,
                            tree,
                            potential_ally,
                        )? {
                            Some(chain) => {
                                return Ok(Some(
                                    [
                                        chain,
                                        vec![(potential_ally.to_owned(), ally_ennemy.to_owned())],
                                    ]
                                    .concat(),
                                ));
                            }
                            None => continue,
                        }
                    }
                    _ => continue,
                };
            }
            return Ok(None);
        }
    }
    Ok(None)
}

fn plots_next(n: u32, tree: &PeopleTree) -> Result<(), ()> {
    let mut stone_safe = true;

    println!("\nConspiracies:");
    let mut chains: Vec<Vec<(String, String)>> = vec![];
    'direct_ennemies: for queen_ennemy in
        tree.get_ennemies("Cersei Lannister").into_iter().flatten()
    {
        if let Some(potential_allies) = tree.get_ennemies(queen_ennemy) {
            let mut potential_allies = potential_allies.collect::<Vec<_>>();
            potential_allies.sort_unstable_by(|l, r| compare_allies(tree, l, r));
            for potential_ally in potential_allies {
                if chains.iter().flatten().any(|(v, _)| v == potential_ally) {
                    continue;
                }
                match tree.separation("Cersei Lannister", potential_ally) {
                    Some(sep) if sep <= n => {
                        // Close Friend
                        chains.push(vec![(potential_ally.to_owned(), queen_ennemy.to_owned())]);
                        continue 'direct_ennemies;
                    }
                    Some(_) => {
                        // Far Friend
                        match try_make_far_friend_ally(
                            &[
                                chains.clone(),
                                vec![vec![(potential_ally.to_owned(), queen_ennemy.to_owned())]],
                            ]
                            .concat(),
                            n,
                            tree,
                            potential_ally,
                        )? {
                            Some(chain) => {
                                chains.push(
                                    [
                                        chain,
                                        vec![(potential_ally.to_owned(), queen_ennemy.to_owned())],
                                    ]
                                    .concat(),
                                );
                                continue 'direct_ennemies;
                            }
                            None => continue,
                        }
                    }
                    _ => continue,
                };
            }
            stone_safe = false;
        } else {
            stone_safe = false;
            break;
        }
    }

    chains.sort_unstable_by(|l, r| (l.first().expect("").0).cmp(&r.first().expect("").0));
    chains.sort_by_key(Vec::len);
    for chain in &chains {
        for chara in chain.iter().rev().skip(1).rev() {
            print!("{} -> {} -> ", chara.0, chara.1);
        }
        print!(
            "{} -> {}",
            chain.last().expect("").0,
            chain.last().expect("").1
        );
        println!();
    }
    for queen_ennemy in tree.get_ennemies("Cersei Lannister").into_iter().flatten() {
        if chains
            .iter()
            .filter_map(|i| i.last())
            .any(|(_, r)| r == queen_ennemy)
        {
            continue;
        }
        println!("No conspiracy possible against {queen_ennemy}");
    }

    println!("\nResult:");
    if stone_safe {
        println!("The stone is safe!");
    } else {
        println!("There is only one way out: treason!");
    }
    Ok(())
}

fn run() -> Result<(), ()> {
    let mut args = std::env::args();
    args.next();

    let Some(select) = args.next() else {
        return Err(());
    };
    if select == "-h" || select == "--help" {
        println!(
            r"USAGE
    ./game_of_stones [--links FR P1 P2 | --plots FR CR n]
DESCRIPTION
    FR file containing friendship relations between people
    Pi name of someone in the friendships file
    CR file containing conspiracies intentions
    n maximum length of friendship paths
"
        );
    }
    if select == "--links" {
        return links(args);
    }
    if select == "--plots" {
        return plots(args);
    }
    if args.next().is_some() {
        return Err(());
    }
    Ok(())
}

enum MyResult {
    Ok,
    Err,
}

impl Termination for MyResult {
    fn report(self) -> ExitCode {
        match self {
            Self::Ok => 0.into(),
            Self::Err => 84.into(),
        }
    }
}

fn main() -> MyResult {
    if run() == Ok(()) {
        MyResult::Ok
    } else {
        println!("ERROR");
        MyResult::Err
    }
}
