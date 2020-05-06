use derive_more::*;

use rand::prelude::*;

use std::fmt;


// Matrix size (up to 16)
const N: usize = 12;

// u16 is used as a column of an NxN binary matrix
#[derive(BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Clone,
         Copy, Eq, PartialEq)]
struct BitMatrixCol(u16);

impl BitMatrixCol {
    /// Initialize a column with one row at 1 and the other rows at 0
    fn one_at(row_idx: usize) -> Self {
        Self(1 << row_idx)
    }

    /// Count how many bits are set in the target column
    fn count_ones(&self) -> u32 {
        self.0.count_ones()
    }
}

impl fmt::Debug for BitMatrixCol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:0width$b}", self.0, width = N)
    }
}

// Number of allowed XOR rounds
const M: usize = 32;

fn main() {
    // Problem statement and correctness check
    println!("Working with matrix size N={} and XOR network length M={}", N, M);
    assert!(N <= 16, "As currently written, this code only works for N <= 16. \
                      (to support more, the matrix column type must change)");


    // === SET UP PROBLEM MATRIX ===

    // Build the identity matrix
    println!("Starting from an identity matrix");
    let mut matrix = (0..N).map(BitMatrixCol::one_at)
                           .collect::<Box<[_]>>();

    // Randomly XOR columns, keeping the matrix invertible
    println!("Applying column XORs...");
    let mut rng = rand::thread_rng();
    for _ in 0..M {
        // Pick two _distinct_ columns at random
        let src_col_idx = rng.gen_range(0, N);
        let src_col = matrix[src_col_idx];
        let dest_col_idx = loop {
            let dest_col_idx = rng.gen_range(0, N);
            if dest_col_idx != src_col_idx { break dest_col_idx; }
        };

        // XOR source column into destination column
        println!("Applying matrix[{}] ^= matrix[{}]", dest_col_idx, src_col_idx);
        matrix[dest_col_idx] ^= src_col;
    }

    // Shuffle matrix columns
    println!("Matrix before shuffling: {:#?}", matrix);
    matrix.shuffle(&mut rng);
    println!("Matrix after shuffling: {:#?}", matrix);


    // === TRYING TO SOLVE THE PROBLEM ===

    // Build a database of (column index, number of bits set) tuples.
    let mut col_idx_and_num_ones =
        matrix.iter()
              .map(BitMatrixCol::count_ones)
              .enumerate()
              .collect::<Box<[_]>>();

    // Sort by descending number of bits
    col_idx_and_num_ones.sort_by(|a, b| b.1.cmp(&a.1));
    println!("(col_idx, num_ones) database: {:?}", col_idx_and_num_ones);

    // Try to find the set of column XORs that we must apply to go to one single
    // bit set per column (which means that we're only one permutation away
    // from the identity matrix).
    let mut num_xors = 0;
    while col_idx_and_num_ones[0].1 > 1 {
        // Ultimately, we want to find the best XOR, that is, the one that
        // clears the most bits from a column of the matrix
        let mut best_dest_db_idx = usize::MAX;
        let mut best_src_col_idx = usize::MAX;
        let mut best_removed_ones = i32::MIN;

        // We investigate "destination" columns, starting from the one that
        // has the largest number of bits set.
        for dest_db_idx in 0..N {
            // Once we reach a destination column that has as many bits set
            // as our best bit-clearing choice pick so far, plus one, we can
            // stop : we won't manage to do any better.
            //
            // If the matrix is invertible, the largest number of bits that we
            // can clear by XORing a "source" column into a "destination" column
            // with X bits set is X-1. We can only clear all X bits from the
            // destination column if the source column is identical, which means
            // that the matrix is not invertible.
            //
            let (dest_col_idx, dest_num_ones) = col_idx_and_num_ones[dest_db_idx];
            if dest_num_ones as i32 <= best_removed_ones + 1 { break; }
            let dest_col = matrix[dest_col_idx];

            // Next, we investigate "source" columns that we could XOR into this
            // destination column, among the ones that have less bits set.
            //
            // The reason why we only need to consider source columns with less
            // or as many bits set is that...
            //
            // - Over the full iteration of dest_db_idx and src_db_idx, we will
            //   consider all possible pairs of columns.
            // - Given a pair of columns where one has more bits set than the
            //   others, it is always as advantageous or more advantageous to do
            //   the XOR into the column that has more bits set (see issue #1
            //   for a mathematical proof).
            //
            // NOTE: I'm not sure if the above property remains true when we
            //       start going deeper in the decision tree and considering
            //       moves composed of two successive XORs. If the code is
            //       reworked in this direction, that logic may need to change.
            //
            for src_db_idx in dest_db_idx+1..N {
                // Once we reached a source column that has less or as much bits
                // set as our best bits-clearing choice so far, we can stop : we
                // won't find a better source column.
                //
                // A source column with Y bits set can only clear up to Y bits
                // in the destination column upon XORing.
                //
                let (src_col_idx, src_num_ones) = col_idx_and_num_ones[src_db_idx];
                if src_num_ones as i32 <= best_removed_ones { break; }
                let src_col = matrix[src_col_idx];

                // Try XORing our destination column with our source column, see
                // how many ones that would remove in the destination column
                let xor_result = dest_col ^ src_col;
                let num_removed_ones =
                    (dest_num_ones as i32) - (xor_result.count_ones() as i32);

                // If that's better than our best guess so far, this source
                // column becomes our new best candidate for XORing into dest
                if num_removed_ones > best_removed_ones {
                    best_dest_db_idx = dest_db_idx;
                    best_src_col_idx = src_col_idx;
                    best_removed_ones = num_removed_ones;
                }
            }
        }

        // At this point, we should have managed to get rid of at least one set
        // bit in the destination column.
        assert!(best_removed_ones > 0,
                "Cannot reduce #ones in any column with any XOR, giving up!");

        // Apply XOR to the matrix
        let best_dest_col_idx = col_idx_and_num_ones[best_dest_db_idx].0;
        println!("Will apply matrix[{}] ^= matrix[{}]",
                 best_dest_col_idx,
                 best_src_col_idx);
        matrix[best_dest_col_idx] ^= matrix[best_src_col_idx];
        println!("Matrix is now: {:#?}", matrix);

        // Update sorted list of column index vs number of ones
        //
        // We could do this more efficiently than by using a general sorting
        // algorithm since the list is mostly sorted, and only the column we
        // just touched must be moved around. But given how low N and M are, we
        // don't really need to speed up this part of the code.
        //
        col_idx_and_num_ones[best_dest_db_idx].1 -= best_removed_ones as u32;
        col_idx_and_num_ones.sort_by(|a, b| b.1.cmp(&a.1));
        println!("(col_idx, num_ones) database: {:?}", col_idx_and_num_ones);

        // Keep track of the number of XORs that we applied
        num_xors += 1;
    }

    // Check if we managed to resolve the problem in few enough XORs.
    assert!(matrix.iter().all(|col| col.count_ones() == 1));
    println!("Reached a permutation of identity matrix in {} XORs", num_xors);
    assert!(num_xors <= M);
}
