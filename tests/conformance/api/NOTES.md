# API mode (`GOBLIN_NONINTERACTIVE=1`)

goblin-host runs a `/api/<name>.gbln` script either through the `goblin` CLI
(interpreter) or in-process on the VM. The response contract is not written
down in `docs/`; these cases pin what the interpreter's runner does, which is
what existing apps (Campfire's `app/api/app.gbln`) rely on:

- Every `say`, and every top-level expression statement of the entry script
  whose value is not nil, unit or empty, is one emission. Its text is the body.
- Status, headers and cookies come from `set_status`, `set_header` and
  `set_cookie`.
- Expressions at the top level of imported modules are not emissions.
- With no emission, nothing is printed.

The VM used to take only `say` output as the body and ignore the final
expression; it now emits the same way (`BuiltinId::ApiEcho`), and
`goblin run --vm` prints the same envelope as `goblin run`.

Known difference not covered by a case: with more than one emission the
interpreter prints one envelope per emission (each snapshotting the status at
that moment), which goblin-host cannot parse as JSON and so serves as plain
text. The VM joins the emissions with newlines into one envelope. Neither is
documented; a script that emits once behaves the same on both.
