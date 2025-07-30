use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::io::BufRead;
use std::ops::Deref;
use std::string::String;

pub fn load_data(file: impl BufRead, sep: &str) -> Result<Vec<(String, String)>, ()> {
    let mut map = vec![];
    let reader = file.lines();
    for line in reader {
        let line = line.map_err(|_| ())?;
        let mut line = line.split(sep);
        map.push((
            line.next().ok_or(())?.to_string(),
            line.next().ok_or(())?.to_string(),
        ));
        if line.next().is_some() {
            return Err(());
        }
    }
    Ok(map)
}

pub struct PeopleTree {
    friendships: BTreeMap<String, BTreeSet<String>>,
    plots: BTreeMap<String, BTreeSet<String>>,
    ennemies: BTreeMap<String, BTreeSet<String>>,
}

impl PeopleTree {
    pub fn new(data: Vec<(String, String)>) -> Result<Self, ()> {
        let mut friendships: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
        for (left, right) in data {
            friendships
                .entry(left.clone())
                .or_default()
                .insert(right.clone());
            friendships.entry(right).or_default().insert(left);
        }

        Ok(Self {
            friendships,
            plots: BTreeMap::default(),
            ennemies: BTreeMap::default(),
        })
    }

    pub fn add_plots(&mut self, data: Vec<(String, String)>) -> Result<&mut Self, ()> {
        for (left, right) in data {
            if !self.friendships.contains_key(&left) || !self.friendships.contains_key(&right) {
                return Err(());
            }
            self.plots
                .entry(left.clone())
                .or_default()
                .insert(right.clone());
            self.ennemies.entry(right).or_default().insert(left);
        }

        Ok(self)
    }

    #[must_use] pub fn separation(&self, p1: &str, p2: &str) -> Option<u32> {
        if p1 == p2 {
            return Some(0);
        }

        let mut visited = BTreeSet::new();
        let mut queue = VecDeque::new();

        queue.push_back((p1, 0));
        visited.insert(p1.to_string());

        while let Some((current, dist)) = queue.pop_front() {
            if let Some(friends) = self.friendships.get(current) {
                for friend in friends {
                    if !visited.contains(friend) {
                        if friend == p2 {
                            return Some(dist + 1);
                        }
                        queue.push_back((friend, dist + 1));
                        visited.insert(friend.to_string());
                    }
                }
            }
        }
        None
    }

    #[must_use] pub fn is_plotting(&self, plotter: &str, target: &str) -> bool {
        self.plots.get(plotter).is_some_and(|v| v.contains(target))
    }

    #[must_use] pub fn all_names_sorted(&self) -> Vec<String> {
        let mut all_names = BTreeSet::new();

        for friends in self.friendships.values() {
            for friend in friends {
                all_names.insert(friend.clone());
            }
        }
        all_names.into_iter().collect()
    }

    #[must_use] pub fn get_ennemies(&self, target: &str) -> Option<impl Iterator<Item = &str>> {
        self.ennemies
            .get(target)
            .map(|v| v.iter().map(Deref::deref))
    }

    #[must_use] pub fn has_ennemy(&self, target: &str, ennemy: &str) -> bool {
        self.ennemies
            .get(target)
            .is_some_and(|ennemies| ennemies.contains(ennemy))
    }
}
