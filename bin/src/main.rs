use shift_tac_toe::*;

use log::{debug, trace};

use std::sync::{Arc, Mutex};

fn main() {
    let env = env_logger::Env::default().filter_or("STT_LOG", "warn");
    env_logger::Builder::from_env(env).init();


    let wld_tree = shift_tac_toe::WinLoseDrawTree::build();
    println!("{:?}", wld_tree.position_count());
    let starting_position = Position::default();
    println!("{:?}", wld_tree.get_position(&starting_position));

//    let mut current_position = Position::default();
//    loop {
//        if current_position.win_state() != WinState::NoWinners {
//            println!("Game is over. {:?}", current_position.win_state());
//            break;
//        }
//
//        if current_position.player() == Player::One {
//            let plays = wld_tree.get_plays_for_position(&current_position).expect(format!("Position {:?} not in WLD tree", current_position.clone()).as_str());
//            let mut maybe_best_play = None;
//            let mut maybe_best_wld_node: Option<Arc<Mutex<WinLoseDrawNode>>> = None;
//            for (play, wld_node_ref) in plays {
//                if let (Some(best_play), Some(best_wld_node)) = (maybe_best_play, maybe_best_wld_node.as_ref()) {
//                    let new_play_is_better;
//                    {
//                        let best_wld_node = best_wld_node.get_lock();
//                        let wld_node = wld_node_ref.get_lock();
//                        debug!("Current best play: {best_play:?}, {best_wld_node:?}");
//                        new_play_is_better = right_is_better_than_left(&best_wld_node, &wld_node);
//                    }
//
//                    if new_play_is_better {
//                        maybe_best_play = Some(play);
//                        maybe_best_wld_node = Some(wld_node_ref);
//                    }
//                } else {
//                    assert!(maybe_best_play.is_none() && maybe_best_wld_node.is_none());
//                    maybe_best_play = Some(play);
//                    maybe_best_wld_node = Some(wld_node_ref);
//                }
//            }
//            let Some(best_play) = maybe_best_play else {
//                panic!("No plays for position {current_position:?}");
//            };
//            current_position = current_position.make_play(best_play).expect("Best play was invalid");
//        } else {
//        }
//    }
}

fn right_is_better_than_left(left: &WinLoseDrawNode, right: &WinLoseDrawNode) -> bool {
    let left_wins = left.player_one_wins();
    let left_losses = left.player_two_wins();
    let left_draws = left.both_win();

    let right_wins = right.player_one_wins();
    let right_losses = right.player_two_wins();
    let right_draws = right.both_win();

    // current goal is to win, maximize that.
    if left_wins < right_wins {
        return true;
    }

    if left_wins == 0 {
        assert_eq!(right_wins, 0);

        // we can't win, so new goal is to maximize draws.
        if left_draws < right_draws {
            return true;
        }

        // if draws are equal, minimize losses
        return left_losses < right_losses;
    }

    // if we can't maximize wins, minimize losses.
    // if we can't minimize losses, draws are both good (relative to losses) and bad (relative to wins), so ignore them
    left_losses > right_losses
}