"""Word module - represents Sanskrit words as sequences."""

from typing import List, Optional, Union, overload


class Word:
    """Represents a Sanskrit word as a sequence of phonetic elements."""
    elements: list[int]

    def __init__(self, elements: Optional[Union[List[int], str, "Word"]] = None):
        """Initialize a word from elements or convert from string."""
        if elements is None:
            self.elements = []
        elif isinstance(elements, Word):
            self.elements = elements.elements.copy()
        # elif isinstance(elements, str):
            # For now, store as-is; encoding will be handled by encode module
            # self.elements = list(elements) if elements else []
        elif isinstance(elements, list):
            self.elements = elements.copy()
        else:
            raise Exception("Invalid type for Word initialization")
            # self.elements = [elements]

    def length(self) -> int:
        """Return the length (number of elements) in the word."""
        return len(self.elements)

    def __len__(self) -> int:
        """Return the length of the word."""
        return len(self.elements)

    def __iter__(self):
        """Iterate over elements in the word."""
        return iter(self.elements)

    @overload
    def __getitem__(self, index: slice) -> Word:
        pass
    @overload
    def __getitem__(self, index: int) -> int:
        pass

    def __getitem__(self, index):
        """Get element or slice of word."""
        if isinstance(index, slice):
            return Word(self.elements[index])
        return self.elements[index]

    def __add__(self, other) -> "Word":
        """Concatenate two words."""
        if isinstance(other, Word):
            return Word(self.elements + other.elements)
        return Word(self.elements + list(other))

    def __eq__(self, other) -> bool:
        """Check equality."""
        if isinstance(other, Word):
            return self.elements == other.elements
        return self.elements == other

    def __repr__(self):
        """String representation."""
        return f"Word({self.elements})"

    def __str__(self) -> str:
        """String output."""
        return "".join(str(e) for e in self.elements)

    def prefix(self, other: "Word") -> bool:
        """Check if this word is a prefix of other."""
        if isinstance(other, Word):
            other_els = other.elements
        else:
            other_els = other

        if len(self.elements) > len(other_els):
            return False
        return other_els[: len(self.elements)] == self.elements

    def mirror(self) -> "Word":
        """Return reversed word (mirror)."""
        return Word(self.elements[::-1])

    def copy(self) -> "Word":
        """Return a copy of the word."""
        return Word(self.elements.copy())

    @staticmethod
    def empty() -> "Word":
        """Create an empty word."""
        return Word([])
