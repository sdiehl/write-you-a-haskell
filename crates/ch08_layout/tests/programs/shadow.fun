{ main = (let { x = 1; } in ((let { x = 2; } in x) + x)); }
