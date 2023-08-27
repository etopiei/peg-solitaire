from manim import *

class StoneSolitaire(Scene):
    def initial_board(self):
        return self.make_group_from_board("""
            xxOOOxx
            xxOOOxx
            OOOOOOO
            OOO_OOO
            OOOOOOO
            xxOOOxx
            xxOOOxx
        """)

    def make_group_from_board(self, board):
        board_string = board.replace(' ', "").replace("\n", "").replace("\r", "")

        rows = 7
        cols = 7
        circle_radius = 0.3

        circles = [] 
        for i in range(rows):
            for j in range(cols):
                index = 7*i+j
                if board_string[index] == "_":
                    circle = Circle(radius=circle_radius, stroke_color=WHITE)
                    circles.append(circle)
                elif board_string[index] == "O":
                    circle = Circle(radius=circle_radius, stroke_color=WHITE, fill_color=WHITE, fill_opacity=1)
                    circles.append(circle)
                else:
                    blank = Circle(radius=circle_radius, stroke_color=BLACK)
                    circles.append(blank)

        circle_group = Group(*circles).arrange_in_grid(rows=rows, cols=cols).scale_to_fit_width(7)
        return circle_group

    def construct(self):
        ending_boards = list(reversed(open("yes.txt").read().split("\n\n")))

        first = True
        previous_position = None
        for board in ending_boards:
            if first:
                board_obj = self.make_group_from_board(board)
                self.add(board_obj)
                previous_position = board_obj
                first = False
            else:
                new_board = self.make_group_from_board(board)
                self.play(TransformMatchingShapes(previous_position, new_board, path_arc=PI/2))
                previous_position = new_board

if __name__ == "__main__":
    scene = StoneSolitaire()
    scene.render()