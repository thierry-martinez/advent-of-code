use advent_of_code::{Error, Result, Coords3D};

struct Block {
    inf: Coords3D<u64>,
    sup: Coords3D<u64>,
}

impl Block {
    fn intersect_xy(&self, other: &Block) -> bool {
        self.inf.x <= other.sup.x && other.inf.x <= self.sup.x &&
        self.inf.y <= other.sup.y && other.inf.y <= self.sup.y
    }
}

impl std::str::FromStr for Block {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        let (inf_str, sup_str) = s.split_once('~').ok_or("'~' missing")?;
        Ok(Self {
            inf: Coords3D::parse(",", inf_str)?,
            sup: Coords3D::parse(",", sup_str)?
        })
    }
}

struct FallenBlock {
    id: usize,
    block: Block,
    support: Vec<usize>,
}

fn filter_support(fallen: &Vec<FallenBlock>, block: &Block) -> (u64, Vec<usize>) {
    let mut max_z = 0;
    let mut max_vec = Vec::new();
    for fallen_block in fallen {
        if !fallen_block.block.intersect_xy(block) {
            continue;
        }
        if max_z < fallen_block.block.sup.z {
            max_z = fallen_block.block.sup.z;
            max_vec.clear();
            max_vec.push(fallen_block.id);
        }
        else if max_z == fallen_block.block.sup.z {
            max_vec.push(fallen_block.id);
        }
    }
    (max_z, max_vec)
}

fn fall_blocks(blocks: &Vec<(usize, Block)>) -> Vec<FallenBlock> {
    let mut fallen = Vec::new();
    for (id, block) in blocks {
        let (z, support) = filter_support(&fallen, &block);
        let z = z + 1;
        let height = block.sup.z - block.inf.z;
        let block = Block {
            inf: Coords3D { z, ..block.inf },
            sup: Coords3D { z: z + height, ..block.sup },
        };
        fallen.push(FallenBlock { id: *id, block, support })
    }
    fallen
}

fn count_fall(
    support: &std::collections::HashMap<usize, std::collections::HashSet<usize>>,
    supported: &std::collections::HashMap<usize, std::collections::HashSet<usize>>,
    id: usize
) -> usize {
    let mut fall_set = std::collections::HashSet::new();
    fall_set.insert(id);
    let mut added_vec: Vec<_> = supported.get(&id).unwrap().iter().collect();
    while let Some(&added) = added_vec.pop() {
        if !fall_set.contains(&added) {
            let support_set = support.get(&added).unwrap();
            if !support_set.is_empty() && support_set.is_subset(&fall_set) {
                fall_set.insert(added);
                if let Some(supported_ids) = supported.get(&added) {
                    added_vec.extend(supported_ids);
                }
            }
        }
    }
    fall_set.len() - 1
}

fn part2(
    blocks: &Vec<FallenBlock>,
    singletons: std::collections::HashSet<usize>
) -> usize {
    let mut support = std::collections::HashMap::new();
    let mut supported = std::collections::HashMap::new();
    for fallen in blocks {
        support.insert(fallen.id, fallen.support.iter().cloned().collect());
        for &support_id in &fallen.support {
            supported.entry(support_id)
                .or_insert(std::collections::HashSet::new())
                .insert(fallen.id);
        }
    }
    singletons.iter().map(|&id| count_fall(&support, &supported, id)).sum()
}

fn main() -> Result<()> {
    let mut blocks: Vec<(usize, Block)> = std::io::stdin().lines()
        .enumerate()
        .map(|(id, line)| -> Result<_> { Ok((id, line?.parse()?)) })
        .collect::<Result<_>>()?;
    blocks.sort_by_key(|(_, block)| block.inf.z);
    let blocks = fall_blocks(&blocks);
    let singletons: std::collections::HashSet<_> = blocks.iter().filter_map(
        |fallen|
        match &fallen.support[..] {
            &[id] => Some(id),
            _ => None,
        }
    ).collect();
    let result_part1 = blocks.len() - singletons.len();
    println!("Part 1: {result_part1}");
    let result_part2 = part2(&blocks, singletons);
    println!("Part 2: {result_part2}");
    Ok(())
}
