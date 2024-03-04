use super::*;

#[derive(Eq, PartialEq)]
enum TriviaState {
    Start,
    InlineComment,
    WhiteSpace,
    BlockComment,
    BlockCommentEnd,
    End,
}
impl<'a, E> Lexer<'a, E>
where
    E: ErrorReporter,
{
    /// Removes whitespace and comments from incoming source
    pub(super) fn remove_trivial(&mut self) {
        let mut state = TriviaState::Start;
        loop {
            if state != TriviaState::Start {
                self.next_char();
            }
            if state == TriviaState::BlockCommentEnd {
                self.next_char();
            }
            state = self.get_current_trivia(state);
            if state == TriviaState::End {
                break;
            }
        }
    }

    fn get_current_trivia(&self, state: TriviaState) -> TriviaState {
        use TriviaState::*;
        match (state, self.current, self.next) {
            (WhiteSpace | BlockCommentEnd, Some(current), _) => Start,

            (Start, Some(current), _) if current.is_whitespace() => WhiteSpace,

            (BlockComment, Some('*'), Some('/')) => BlockCommentEnd,

            (Start, Some('/'), Some('*')) | (BlockComment, Some(_), _) => BlockComment,

            (InlineComment, Some('\n'), _) => Start,

            (Start, Some('/'), Some('/')) | (InlineComment, Some(_), _) => InlineComment,

            _ => End,
        }
    }
}
