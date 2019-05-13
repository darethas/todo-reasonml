module TodoAppItem = {
  [@react.component]
  let make = (~task, ~checked, ~onChange) => {
    <li className="mb-1">
      <input
        type_="checkbox"
        className="border rounded-full mr-2"
        checked
        onChange
      />
      <p className="font-mono text-lg inline-block">
        {checked
           ? <s> {ReasonReact.string(task)} </s> : ReasonReact.string(task)}
      </p>
    </li>;
  };
};

module TodoApp = {
  type todo = {
    name: string,
    mutable complete: bool,
  };

  type formToggle =
    | ShowButton
    | ShowForm;

  type state = {
    todos: array(todo),
    formToggle,
    input: ref(option(Dom.element)),
  };

  type action =
    | AddTodo(string)
    | ToggleTodo(int)
    | ToggleForm;

  [@react.component]
  let make = () => {
    let (state, dispatch) =
      React.useReducer(
        (state, action) =>
          switch (action) {
          | AddTodo(task) => {
              ...state,
              todos:
                Array.append(
                  state.todos,
                  [|{name: task, complete: false}|],
                ),
            }
          | ToggleTodo(idx) =>
            let newTodos = Array.copy(state.todos);
            newTodos[idx].complete = !newTodos[idx].complete;
            {...state, todos: newTodos};
          | ToggleForm =>
            let newFormToggle =
              switch (state.formToggle) {
              | ShowForm => ShowButton
              | ShowButton => ShowForm
              };
            {...state, formToggle: newFormToggle};
          },
        {todos: [||], formToggle: ShowButton, input: ref(None)},
      );

    let inputRef = React.useRef(Js.Nullable.null);

    let change = (idx, _evt) => {
      dispatch(ToggleTodo(idx));
    };

    let handleAdd = _e => {
      switch (inputRef->React.Ref.current->Js.Nullable.toOption) {
      | None => ()
      | Some(r) =>
        let v = ReactDOMRe.domElementToObj(r)##value;
        if (v != "") {
          dispatch(AddTodo(v));
          dispatch(ToggleForm);
        };
      };
    };

    let showForm = () => {
      <div>
        <label className="font-mono text-lg">
          {ReasonReact.string("Task Name: ")}
        </label>
        <input
          type_="text"
          ref={ReactDOMRe.Ref.domRef(inputRef)}
          className="font-mono bg-white border shadow-md rounded mr-2 px-1 py-1"
        />
        <button type_="button" onClick=handleAdd className="btn">
          {ReasonReact.string("Add")}
        </button>
      </div>;
    };

    <div className="container mx-auto text-center w-full">
      <h1 className="text-5xl font-mono">
        {ReasonReact.string("Things To Do")}
      </h1>
      <ul>
        {Array.mapi(
           (idx, item) =>
             <TodoAppItem
               key={string_of_int(idx) ++ "-" ++ item.name}
               task={item.name}
               checked={item.complete}
               onChange={change(idx)}
             />,
           state.todos,
         )
         ->ReasonReact.array}
      </ul>
      {switch (state.formToggle) {
       | ShowButton =>
         <button
           type_="button"
           onClick={_evt => dispatch(ToggleForm)}
           className="btn mt-4">
           {ReasonReact.string("Add Todo")}
         </button>
       | ShowForm => showForm()
       }}
    </div>;
  };
};

ReactDOMRe.renderToElementWithId(<TodoApp />, "app");