use log::{info, debug, trace, warn};

use std::collections::HashMap;
use std::sync::{Arc, Mutex, MutexGuard};


#[derive(Debug, Default, Clone, Copy, Hash, PartialEq, Eq)]
pub enum RowPosition {
    Left,
    #[default]
    Center,
    Right
}

impl RowPosition {
    pub fn shift_left(self) -> Option<Self> {
        match self {
            Self::Left => None,
            Self::Center => Some(Self::Left),
            Self::Right => Some(Self::Center),
        }
    }

    pub fn shift_right(self) -> Option<Self> {
        match self {
            Self::Left => Some(Self::Center),
            Self::Center => Some(Self::Right),
            Self::Right => None,
        }
    }
}

// Conclusion represents the the eventual outcome of play between two perfect players.
// If an individual board has a winner, it has a WinState.
// The difference is that there can be no draw on an individual board;
// a pair of perfect players might draw by continuously choosing the same sequence of play.

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Conclusion {
    Draw,
    PlayerOneWins,
    PlayerTwoWins,
    PlayersOneAndTwoWin,
}

impl From<Player> for Conclusion {
    fn from(player: Player) -> Self {
        match player {
            Player::One => Self::PlayerOneWins,
            Player::Two => Self::PlayerTwoWins,
        }
    }
}

impl From<WinState> for Conclusion {
    fn from(win_state: WinState) -> Self {
        match win_state {
            WinState::PlayerOne => Self::PlayerOneWins,
            WinState::PlayerTwo => Self::PlayerTwoWins,
            WinState::PlayersOneAndTwo => Self::PlayersOneAndTwoWin,
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum WinState {
    PlayerOne,
    PlayerTwo,
    PlayersOneAndTwo,
}

impl From<Player> for WinState {
    fn from(player: Player) -> Self {
        match player {
            Player::One => Self::PlayerOne,
            Player::Two => Self::PlayerTwo,
        }
    }
}

trait WinStateEx {
    fn with_player_winning(self, player: Player) -> WinState;
}
impl WinStateEx for Option<WinState> {
    fn with_player_winning(self, player: Player) -> WinState {
        match (self, player) {
            (None, p) => p.into(),
            (Some(WinState::PlayerOne), Player::Two) => WinState::PlayersOneAndTwo,
            (Some(WinState::PlayerTwo), Player::One) => WinState::PlayersOneAndTwo,
            (Some(s), _) => s
        }
    }
}

impl Conclusion {
    pub fn choose_best_for_player(player: Player, left: Self, right: Self) -> Self {
        let best_conclusion = Conclusion::from(player);
        match (left, right) {
            (s, _) if s == best_conclusion => best_conclusion,
            (_, s) if s == best_conclusion => best_conclusion,
            // The order of these two clauses is up to interpretation.
            // I'm aiming for games to have a concrete conclusion.
            (_, Self::PlayersOneAndTwoWin) => Self::PlayersOneAndTwoWin,
            (Self::PlayersOneAndTwoWin, _) => Self::PlayersOneAndTwoWin,
            (_, Self::Draw) => Self::Draw,
            (Self::Draw, _) => Self::Draw,
            (s, _) => s
        }
    }
}


#[derive(Debug, Default, Clone, Hash, PartialEq, Eq)]
pub struct Grid {
  array: [[Option<Player>; COLUMN_COUNT]; ROW_COUNT]
}

impl Grid {
    pub fn shift_left(&mut self, row: usize) {
        assert!(row < ROW_COUNT);

        for column in 0..COLUMN_COUNT {
            self.array[row][column] = self.array[row].get(column + 1).and_then(|p| p.clone());
        }

        self.apply_gravity();
        self.validate();
    }

    pub fn shift_right(&mut self, row: usize) {
        assert!(row < ROW_COUNT);

        for column in (0..COLUMN_COUNT).rev() {
            self.array[row][column] =
                if column != 0 {
                    self.array[row][column - 1]
                } else {
                    None
                };
        }

        self.apply_gravity();
        self.validate();
    }

    pub fn drop_piece(&mut self, column: usize, player: Player) -> Option<usize> {
        assert!(column < COLUMN_COUNT);

        let mut maybe_row_to_set = None;

        for row in (0..ROW_COUNT).rev() {
            if self.array[row][column].is_none() {
                maybe_row_to_set = Some(row);
                break;
            }
        }

        if let Some(row_to_set) = maybe_row_to_set {
            self.array[row_to_set][column] = Some(player);
            self.validate();
        }

        maybe_row_to_set
    }

    fn apply_gravity(&mut self) {
        trace!("GRID: Applying gravity to grid {self:?}");
        for column in 0..COLUMN_COUNT {
            trace!("GRID: Applying gravity to column {column}");
            let mut first_empty_row = None;
            for row in (1..ROW_COUNT).rev() {
                trace!("GRID: Checking if piece {row},{column} is empty");
                if self.array[row][column] == None {
                    trace!("GRID: Piece {row},{column} is empty");
                    first_empty_row = Some(row);
                    break;
                }
            }
            let Some(first_empty_row) = first_empty_row else {
                debug!("GRID: No empty piece in column {column}");
                continue;
            };
            trace!("GRID: First empty row was {first_empty_row}");
            for row in (1..=first_empty_row).rev() {
                trace!("GRID: Copying {},{column} ({:?}) to {row},{column} ({:?})", row - 1, self.array[row - 1][column], self.array[row][column]);
                self.array[row][column] = self.array[row - 1][column];
            }

            trace!("GRID: Setting 0,{column} to empty");
            self.array[0][column] = None;
        }
    }

    fn validate(&self) {
        //TODO
    }

    pub fn win_state(&self) -> Option<WinState> {
        let mut win_state = None;

        for row in 0..ROW_COUNT {
            let first_piece = self.array[row][0];
            let mut all_equal = true;
            for column in 1..COLUMN_COUNT {
                all_equal &= first_piece == self.array[row][column];
            }
            if let Some(player) = first_piece.filter(|_| all_equal) {
                win_state = Some(win_state.with_player_winning(player));
            }
        }

        for column in 0..COLUMN_COUNT {
            let first_piece = self.array[0][column];
            let mut all_equal = true;
            for row in 1..ROW_COUNT {
                all_equal &= first_piece == self.array[row][column];
            }
            if let Some(player) = first_piece.filter(|_| all_equal) {
                win_state = Some(win_state.with_player_winning(player));
            }
        }

        if ROW_COUNT != COLUMN_COUNT {
          unimplemented!("Having unequal ROW_COUNT and COLUMN_COUNT is not implemented");
        }

        let top_left_piece = self.array[0][0];
        let mut all_down_slope_equal = true;
        for row_column in 1..ROW_COUNT {
            all_down_slope_equal &= top_left_piece == self.array[row_column][row_column];
        }
        if let Some(player) = top_left_piece.filter(|_| all_down_slope_equal) {
            win_state = Some(win_state.with_player_winning(player));
        }

        let bottom_left_piece = self.array[ROW_COUNT - 1][0];
        let mut all_up_slope_equal = true;
        for row in 0..(ROW_COUNT-1) {
            let column = COLUMN_COUNT - row - 1;
            all_up_slope_equal &= bottom_left_piece == self.array[row][column];
        }
        if let Some(player) = bottom_left_piece.filter(|_| all_up_slope_equal) {
            win_state = Some(win_state.with_player_winning(player));
        }

        win_state
    }
}

#[derive(Debug, Default, Clone, Hash, PartialEq, Eq)]
pub struct Board {
  grid: Grid,
  row_positions: [RowPosition; 3],
  win_state: Option<WinState>
}

impl Board {
    fn new(grid: Grid, row_positions: [RowPosition; 3]) -> Self {
        let win_state = grid.win_state();

        Self {
            grid,
            row_positions,
            win_state,
        }
    }

    pub fn make_play(&self, player: Player, play: Play) -> Option<Self> {
        match play {
            Play::ShiftLeft { row } => {
                assert!(row < ROW_COUNT);

                let mut new_row_positions = self.row_positions.clone();
                new_row_positions[row] = match self.row_positions[row].shift_left() {
                    Some(p) => p,
                    None => return None,
                };

                let mut new_grid = self.grid.clone();
                new_grid.shift_left(row);

                Some(Self::new(new_grid, new_row_positions))
            },
            Play::ShiftRight { row } => {
                assert!(row < ROW_COUNT);

                let mut new_row_positions = self.row_positions.clone();
                new_row_positions[row] = match self.row_positions[row].shift_right() {
                    Some(p) => p,
                    None => return None,
                };

                let mut new_grid = self.grid.clone();
                new_grid.shift_right(row);

                Some(Self::new(new_grid, new_row_positions))
            },
            Play::DropPiece { column } => {
                assert!(column < COLUMN_COUNT);

                let mut new_grid = self.grid.clone();
                if new_grid.drop_piece(column, player).is_none() {
                    return None;
                }

                Some(Self::new(new_grid, self.row_positions.clone()))
            },
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Play {
    ShiftLeft { row: usize },
    ShiftRight { row: usize },
    DropPiece { column: usize },
}
pub const ROW_COUNT: usize = 3;
pub const COLUMN_COUNT: usize = 3;
static ALL_PLAYS: [Play; (ROW_COUNT * 2) + COLUMN_COUNT] = [
    Play::ShiftLeft { row: 0 },
    Play::ShiftLeft { row: 1 },
    Play::ShiftLeft { row: 2 },
    Play::ShiftRight { row: 0 },
    Play::ShiftRight { row: 1 },
    Play::ShiftRight { row: 2 },
    Play::DropPiece { column: 0 },
    Play::DropPiece { column: 1 },
    Play::DropPiece { column: 2 },
];

//pub const ROW_COUNT: usize = 2;
//pub const COLUMN_COUNT: usize = 2;
//static ALL_PLAYS: [Play; (ROW_COUNT * 2) + COLUMN_COUNT] = [
//    Play::ShiftLeft { row: 0 },
//    Play::ShiftLeft { row: 1 },
//    Play::ShiftRight { row: 0 },
//    Play::ShiftRight { row: 1 },
//    Play::DropPiece { column: 0 },
//    Play::DropPiece { column: 1 },
//];

impl Play {
    pub fn all_plays() -> std::slice::Iter<'static, Play> {
        ALL_PLAYS.iter()
    }

    pub fn invert(self) -> Option<Self> {
        match self {
            Self::ShiftLeft { row } => Some(Self::ShiftRight { row }),
            Self::ShiftRight { row } => Some(Self::ShiftLeft { row }),
            Self::DropPiece { .. } => None,
        }
    }
}


#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Player {
    One, Two
}

impl Player {
    pub fn next(self) -> Self {
        match self {
            Self::One => Self::Two,
            Self::Two => Self::One,
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Position {
    board: Board,
    player: Player
}

impl Default for Position {
    fn default() -> Self {
        Self { board: Default::default(), player: Player::One }
    }
}

impl Position {
    // TODO: this should be Result<Position, T>, not Option<T>
    pub fn make_play(&self, play: Play) -> Option<Position> {
        let new_board = match self.board.make_play(self.player, play) {
            Some(b) => b,
            None => return None,
        };
        let new_player = self.player.next();
        Some(
            Self {
                board: new_board,
                player: new_player,
            }
        )
    }

    pub fn player(&self) -> Player {
        self.player
    }

    pub fn win_state(&self) -> Option<WinState> {
        self.board.win_state
    }

    pub fn as_2d_string(&self) -> String {
        let mut result = String::new();

        for _ in 0..(COLUMN_COUNT+4) {
            result += "-";
        }
        for row in 0..ROW_COUNT {
            result += "\n";

            if self.board.row_positions[row] == RowPosition::Left {
                result += "|"
            } else {
                result += " "
            }
            result += "|";

            for column in 0..COLUMN_COUNT {
                result += match self.board.grid.array[row][column] {
                    None => "0",
                    Some(Player::One) => "1",
                    Some(Player::Two) => "2",
                }
            }

            result += "|";
            if self.board.row_positions[row] == RowPosition::Right {
                result += "|"
            } else {
                result += " "
            }
        }
        result += "\n";
        for _ in 0..(COLUMN_COUNT+4) {
            result += "-";
        }
        result += "\n";
        result += match self.board.win_state {
            None => match self.player {
                Player::One => "1's turn",
                Player::Two => "2's turn",
            },
            Some(WinState::PlayerOne) => "1 wins",
            Some(WinState::PlayerTwo) => "2 wins",
            Some(WinState::PlayersOneAndTwo) => "both win",
        };
        result += "\n";

        result
    }
}

#[derive(Default, Clone)]
pub struct WinLoseDrawNode {
    parent: Option<Arc<Mutex<WinLoseDrawNode>>>,
    children: Vec<Arc<Mutex<WinLoseDrawNode>>>,
    tallied: bool,
    position: Position,
    depth: u16,
    untallied_children: u8,
    player_one_wins: u32,
    player_two_wins: u32,
    both_win: u32,
    conclusion: Option<Conclusion>,
}

impl WinLoseDrawNode {
    pub fn new(parent: Arc<Mutex<WinLoseDrawNode>>, position: Position, parent_depth: u16) -> Arc<Mutex<Self>> {
        let (player_one_wins, player_two_wins, both_win) = match position.board.win_state {
            None => (0, 0, 0),
            Some(WinState::PlayerOne) => (1, 0, 0),
            Some(WinState::PlayerTwo) => (0, 1, 0),
            Some(WinState::PlayersOneAndTwo) => (0, 0, 1),
        };
        let conclusion = position.board.win_state.map(Conclusion::from);
        Arc::new(Mutex::new(
            WinLoseDrawNode {
                parent: Some(parent),
                position,
                depth: parent_depth + 1,
                player_one_wins,
                player_two_wins,
                both_win,
                conclusion,
                ..Default::default()
            }
        ))
    }


    pub fn parent(&self) -> Option<&Arc<Mutex<WinLoseDrawNode>>> {
        self.parent.as_ref()
    }

    pub fn position(&self) -> &Position {
        &self.position
    }

    pub fn depth(&self) -> u16 {
        self.depth
    }

    pub fn untallied_children(&self) -> u8 {
        self.untallied_children
    }

    pub fn player_one_wins(&self) -> u32 {
        self.player_one_wins
    }

    pub fn player_two_wins(&self) -> u32 {
        self.player_two_wins
    }

    pub fn both_win(&self) -> u32 {
        self.both_win
    }

    pub fn as_stats_string(&self) -> String {
        format!(
            "{{depth: {}, untallied_children: {}, player_one_wins: {}, player_two_wins: {}, both_win: {}, conclusion: {:?}}}",
            self.depth,
            self.untallied_children,
            self.player_one_wins,
            self.player_two_wins,
            self.both_win,
            self.conclusion,
        )
    }

    // tally win/lose/draws from the bottom up.
    // leafs should all have parents, untallied_children == 0, and one of player_one_wins, player_two_wins, both_wins == 1, with the others set to 0.
    // each non-leaf should start with a untallied_children equal to the >0 number of children it has, and all of the win state counts equal to 0.
    // we start with all the leafs, adding their win state to their parents; when a parent has untallied_children == 0, all of its children should have been tallied up.
    // so we have the parent node add its tally to its parent, check if all its children have been tallied, etc, until we reach the starting state.
    // at the same time, we keep track of how "perfect" players would play against each other in a similar fashion. Instead of keeping counts,
    // we record the best choice for each position, and the parent nodes record the best among those choices, etc.
    pub(crate) fn tally_recursive(&mut self) {
        let mut maybe_current_node_ref = self.tally_individual();
        while let Some(current_node_ref) = maybe_current_node_ref {
            maybe_current_node_ref = current_node_ref.get_lock().tally_individual();
        }
    }

    pub(crate) fn tally_individual(&mut self) -> Option<Arc<Mutex<WinLoseDrawNode>>> {
        trace!("TALLY: tallying node {}\n{}", self.as_stats_string(), self.position.as_2d_string());

        let Some(parent_node_ref) = self.parent.as_ref() else {
            trace!("TALLY: no parent node");
            return None;
        };
        let mut parent_node = parent_node_ref.get_lock();
        trace!("TALLY: parent node before: {}", parent_node.as_stats_string());

        parent_node.player_one_wins += self.player_one_wins;
        parent_node.player_two_wins += self.player_two_wins;
        parent_node.both_win += self.both_win;
        let Some(self_conclusion) = self.conclusion else {
            panic!("Tallying {self:?} with no conclusion.");
        };
        parent_node.conclusion =
            if let Some(parent_conclusion) = parent_node.conclusion {
                Some(Conclusion::choose_best_for_player(parent_node.position.player, parent_conclusion, self_conclusion))
            } else {
                Some(self_conclusion)
            };

        assert_ne!(parent_node.untallied_children, 0, "Tallying a child, but parent_node.untallied_children == 0");
        parent_node.untallied_children -= 1;
        self.tallied = true;
        trace!("TALLY: parent node after: {}", parent_node.as_stats_string());
        if parent_node.untallied_children == 0 {
            if parent_node.conclusion != Some(Conclusion::Draw) {
                assert!(parent_node.player_one_wins + parent_node.player_two_wins + parent_node.both_win != 0, "Non-draw node had 0 untallied children and no win tallies");
            }
            trace!("TALLY: parent has no untallied children, returning to be tallied.");
            Some(parent_node_ref.clone())
        } else {
            None
        }
    }

}

impl std::fmt::Debug for WinLoseDrawNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("WinLoseDrawNode")
          .field("parent", &self.parent.as_ref().map(|_| ())) // don't print parents recursively
          .field("position", &self.position)
          .field("depth", &self.depth)
          .field("untallied_children", &self.untallied_children)
          .field("player_one_wins", &self.player_one_wins)
          .field("player_two_wins", &self.player_two_wins)
          .field("both_win", &self.both_win)
          .field("conclusion", &self.conclusion)
          .finish()
    }
}

pub trait MutexExt<T> {
    fn get_lock<'this>(&'this self) -> MutexGuard<'this, T>;
}

impl<T> MutexExt<T> for Mutex<T> {
    fn get_lock<'this>(&'this self) -> MutexGuard<'this, T> {
        self.lock().expect("Mutex was poisoned")
    }
}

#[derive(Debug, Clone)]
pub struct WinLoseDrawTree {
    nodes: HashMap<Position, Arc<Mutex<WinLoseDrawNode>>>,
    leafs: Vec<Arc<Mutex<WinLoseDrawNode>>>,
    // nodes without a win state, but all children are positions that already exist
    // pseudo_leafs: HashMap<Position, Arc<Mutex<WinLoseDrawNode>>>,
}

impl WinLoseDrawTree {
    pub fn build() -> Self {
        let mut result = Self::construct();
        result.tally_wins();
        result
    }

    pub fn position_count(&self) -> usize {
        self.nodes.len()
    }

    pub fn get_position(&self, position: &Position) -> Option<&Arc<Mutex<WinLoseDrawNode>>> {
        self.nodes.get(position)
    }

    pub fn get_plays_for_position(&self, position: &Position) -> Option<Vec<(Play, Arc<Mutex<WinLoseDrawNode>>)>> {
        let mut result = vec![];

        for play in Play::all_plays() {
            let play = *play;
            let Some(child_position) = position.make_play(play) else {
                info!("Play {play:?} is invalid, skipping.");
                continue;
            };
            // TODO: this should probably return an error rather than panicking
            let child_node = self.nodes.get(&child_position).expect("Could not get node for position {position:?}");
            result.push((play, child_node.clone()));
        }

        Some(result)
    }

    fn construct() -> Self {
        let mut result = Self { nodes: Default::default(), leafs: Default::default(), /*pseudo_leafs: Default::default()*/ };
        let root_node = Arc::new(Mutex::new(WinLoseDrawNode::default()));
        let mut node_stack = vec![root_node.clone()];
        result.nodes.insert(Position::default(), root_node);
        let mut has_valid_play = false;
        while let Some(current_node_ref) = node_stack.pop() {
            let mut current_node = current_node_ref.get_lock();
            trace!("CONSTRUCTION: Constructing from\n{}", current_node.position.as_2d_string());
            let current_position = current_node.position.clone();
            let plays = Play::all_plays();
            for play in plays {
                // filter out invalid plays
                let Some(child_position) = current_position.make_play(*play) else {
                    trace!("CONSTRUCTION: {play:?} would be invalid");
                    continue;
                };
                has_valid_play = true;

                //filter out positions we've already seen.
                if result.nodes.contains_key(&child_position) {
                    trace!("CONSTRUCTION: {play:?} results in an already-existing position.");
                    continue;
                }

                let child_node: Arc<Mutex<WinLoseDrawNode>> = WinLoseDrawNode::new(current_node_ref.clone(), child_position.clone(), current_node.depth);
                current_node.untallied_children += 1;
                current_node.children.push(child_node.clone());
                result.nodes.insert(child_position.clone(), child_node.clone());

                if child_position.board.win_state != None {
                    trace!("CONSTRUCTION: {play:?} results in a leaf\n{}", child_node.get_lock().position.as_2d_string());
                    result.leafs.push(child_node.clone());
                } else {
                    node_stack.push(child_node);
                }
            }
            assert!(has_valid_play);
            if current_position.board.win_state == None && current_node.children.len() == 0 {
                trace!("CONSTRUCTION: {current_node:?} is a draw.");
                current_node.conclusion = Some(Conclusion::Draw);
                result.leafs.push(current_node_ref.clone());
            }
        }
        result
    }

    fn tally_wins(&mut self) {
        for leaf_ref in &self.leafs {
            trace!("TALLY: tallying leaf");
            leaf_ref.get_lock().tally_recursive();
        }

        Self::check_tallies(self.nodes[&Position::default()].clone());
    }

    fn check_tallies(node_ref: Arc<Mutex<WinLoseDrawNode>>) {
        let mut node_ref_stack = vec![node_ref];
        while let Some(node_ref) = node_ref_stack.pop() {
            let node = node_ref.get_lock();
            if !node.tallied {
                warn!("TALLY: {node:?} not tallied");
            }
            if node.untallied_children != 0 {
                warn!("TALLY: {node:?} has {} untallied_children.", node.untallied_children);
            }
            let mut untallied_children = 0u8;
            for child_ref in node.children.as_slice() {
                let child = child_ref.get_lock();
                if !child.tallied {
                  warn!("TALLY: {node:?} child {child:?} not tallied");
                  untallied_children += 1;
                }
            }
            if untallied_children != node.untallied_children {
                warn!("TALLY: counted {} untallied children, {} was the stored count", untallied_children, node.untallied_children);
            }
            for child_ref in node.children.as_slice() {
                node_ref_stack.push(child_ref.clone());
            }
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[cfg(test)]
    #[ctor::ctor]
    fn setup_logging() {
        simple_logger::init_with_level(log::Level::Warn).expect("could not setup logging");
    }

    #[test]
    pub fn fill_left_column() {
        let position = Position::default();
        let play = Play::DropPiece { column: 0 };
        let Some(position) = position.make_play(play) else {
            panic!("{play:?} on {position:?} was regarded as invalid.");
        };

        assert_eq!(position,
          Position {
            board: Board {
                grid: Grid {
                    array:
                    [
                        [None, None, None],
                        [None, None, None],
                        [Some(Player::One), None, None],
                    ],
                },
                row_positions: [
                    RowPosition::Center,
                    RowPosition::Center,
                    RowPosition::Center,
                ],
                win_state: None
            },
            player: Player::Two
        });

        let Some(position) = position.make_play(play) else {
            panic!("{play:?} on {position:?} was regarded as invalid.");
        };

        assert_eq!(position,
          Position {
            board: Board {
                grid: Grid {
                    array:
                    [
                        [None, None, None],
                        [Some(Player::Two), None, None],
                        [Some(Player::One), None, None],
                    ],
                },
                row_positions: [
                    RowPosition::Center,
                    RowPosition::Center,
                    RowPosition::Center,
                ],
                win_state: None
            },
            player: Player::One
        });

        let Some(position) = position.make_play(play) else {
            panic!("{play:?} on {position:?} was regarded as invalid.");
        };

        assert_eq!(position,
          Position {
            board: Board {
                grid: Grid {
                    array:
                    [
                        [Some(Player::One), None, None],
                        [Some(Player::Two), None, None],
                        [Some(Player::One), None, None],
                    ],
                },
                row_positions: [
                    RowPosition::Center,
                    RowPosition::Center,
                    RowPosition::Center,
                ],
                win_state: None
            },
            player: Player::Two
        });
    }

    #[test]
    fn bottom_row_left_shift_drops_top_two_pieces_on_left_column() {
        let board = Board {
            grid: Grid {
                array:
                [
                    [Some(Player::One), None, None],
                    [Some(Player::Two), None, None],
                    [Some(Player::One), None, None],
                ],
            },
            row_positions: [
                RowPosition::Center,
                RowPosition::Center,
                RowPosition::Center,
            ],
            win_state: None
        };
        let play = Play::ShiftLeft { row: 2 };

        let Some(board) = board.make_play(Player::One, play) else {
            panic!("{play:?} on {board:?} was regarded as invalid.");
        };

        assert_eq!(
            board,
            Board {
                grid: Grid {
                    array:
                    [
                        [None, None, None],
                        [Some(Player::One), None, None],
                        [Some(Player::Two), None, None],
                    ],
                },
                row_positions: [
                    RowPosition::Center,
                    RowPosition::Center,
                    RowPosition::Left,
                ],
                win_state: None
            }
        )
    }

    #[test]
    fn up_slope_regression() {
        assert_eq!(
            Grid {
                array: [
                    [None, None, None],
                    [None, Some(Player::One), None],
                    [Some(Player::One), None, None],
                ]
            }.win_state(),
            None
        );

        assert_eq!(
            Grid {
                array: [
                    [None, None, Some(Player::One)],
                    [None, Some(Player::One), None],
                    [Some(Player::One), None, None],
                ]
            }.win_state(),
            Some(WinState::PlayerOne)
        );
    }

    #[test]
    fn first_node_regression() {
        let tree = WinLoseDrawTree::build();
        let first_position = Position::default();
        let first_node = tree.get_position(&first_position).unwrap();
        assert_eq!(first_node.get_lock().depth, 0);
    }
}
