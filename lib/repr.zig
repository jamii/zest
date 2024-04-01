pub const Repr = union(enum) {
    i32,

    pub fn emptyStruct() Repr {
        // TODO
        return .i32;
    }

    pub fn equal(self: Repr, other: Repr) bool {
        switch (self) {
            .i32 => return other == .i32,
        }
    }
};
