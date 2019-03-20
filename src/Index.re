module TodoAppItem = {
  let component = ReasonReact.statelessComponent("TodoAppItem");
  let make = (~task, ~checked, ~onChange, _children) => {
    ...component,
    render: _self => {
      <li>
        {checked ? <del> {ReasonReact.string(task)} </del> : ReasonReact.string(task)}
        <input type_="checkbox" checked onChange />
      </li>;
    },
  };
};

module TodoApp = {
  type stateRecord = {
    name: string,
    mutable complete: bool,
  };

  type formToggle =
    | ShowButton
    | ShowForm;

  type state = {
    todos: array(stateRecord),
    formToggle,
    inputRef: ref(option(Dom.element)),
  };

  type action =
    | AddTodo(string)
    | ToggleTodo(int)
    | ToggleForm;

  let component = ReasonReact.reducerComponent("TodoApp");

  let make = _children => {
    let change = (idx, _evt, self) => self.ReasonReact.send(ToggleTodo(idx));
    let setRef = (theRef, {ReasonReact.state}) => {
      state.inputRef := Js.Nullable.toOption(theRef);
    };
    let handleAdd = (_evt, self) => {
      switch (self.ReasonReact.state.inputRef^) {
      | None => ()
      | Some(r) =>
        let t = ReactDOMRe.domElementToObj(r);
        let value = t##value;
        if (value == "") {
          ();
        } else {
          self.send(AddTodo(t##value));
          self.send(ToggleForm);
        };
      };
    };
    let showForm = self => {
      <div>
        <label> {ReasonReact.string("Task Name: ")} </label>
        <input type_="text" ref={self.ReasonReact.handle(setRef)} />
        <button type_="button" onClick={self.handle(handleAdd)}> {ReasonReact.string("Add")} </button>
      </div>;
    };
    {
      ...component,

      initialState: () => {
        todos: [|{name: "Add some todos", complete: false}|],
        formToggle: ShowButton,
        inputRef: ref(None),
      },

      reducer: (action, state) => {
        switch (action) {
        | AddTodo(task) =>
          ReasonReact.Update({...state, todos: Array.append(state.todos, [|{name: task, complete: false}|])})
        | ToggleTodo(idx) =>
          state.todos[idx].complete = !state.todos[idx].complete;
          ReasonReact.Update(state);
        | ToggleForm =>
          let newFormToggle =
            switch (state.formToggle) {
            | ShowForm => ShowButton
            | ShowButton => ShowForm
            };
          ReasonReact.Update({...state, formToggle: newFormToggle});
        };
      },

      render: self => {
        <div>
          <h1> {ReasonReact.string("Todo:")} </h1>
          <ul>
            {Array.mapi(
               (idx, item) =>
                 <TodoAppItem
                   key={string_of_int(idx) ++ "-" ++ item.name}
                   task={item.name}
                   checked={item.complete}
                   onChange={self.handle(change(idx))}
                 />,
               self.state.todos,
             )
             ->ReasonReact.array}
          </ul>
          {switch (self.state.formToggle) {
           | ShowButton =>
             <button type_="button" onClick={_evt => self.send(ToggleForm)}>
               {ReasonReact.string("Add Todo")}
             </button>
           | ShowForm => showForm(self)
           }}
        </div>;
      },
    };
  };
};
ReactDOMRe.renderToElementWithId(<TodoApp />, "app");
