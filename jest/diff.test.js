test('diff two objects', () => {
  const a = {
      first: 1,
      second: 2,
      third: 3
  }

  const b = {
      first: 1,
      second: 4,
      third: 3
  }

  expect(a).toBe(b);
});

test('diff two awkward objects', () => {
                    const a =
                        { just: "Just 1"
                        , nothing: "Nothing"
                        , otherJust: "Just 2"
                        }

                    const b =
                        { just: "Nothing"
                        , nothing: "Just 3"
                        , otherJust: "Nothing"
                        }

                    const c =
                        { just: a
                        , nothing: "Nothing"
                        , otherJust: b
                        }

                    const d =
                        { just: "Nothing"
                        , nothing: a
                        , otherJust: b
                        }

  expect(c).toBe(d);
});
