pub const Repr = union(enum) {
    i32,

    pub fn emptyStruct() Repr {
        // TODO
        return .i32;
    }
};
