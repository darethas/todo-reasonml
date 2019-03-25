module TodoAppItem = {
  let component = ReasonReact.statelessComponent("TodoAppItem");
  let make = (~task, ~checked, ~onChange, _children) => {
    ...component,
    render: _self => {
      <li className="mb-1">
        <input type_="checkbox" className="border rounded-full mr-2" checked onChange />
        <p className="font-mono text-lg inline-block">
          {checked ? <s> {ReasonReact.string(task)} </s> : ReasonReact.string(task)}
        </p>
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
        <label className="font-mono text-lg"> {ReasonReact.string("Task Name: ")} </label>
        <input
          type_="text"
          ref={self.ReasonReact.handle(setRef)}
          className="font-mono bg-white border shadow-md rounded mr-2 px-1 py-1"
        />
        <button type_="button" onClick={self.handle(handleAdd)} className="btn">
          {ReasonReact.string("Add")}
        </button>
      </div>;
    };
    {
      ...component,

      initialState: () => {
        todos: [|{name: "some todo", complete: false}|],
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
        <div className="container mx-auto text-center w-full">
          <h1 className="text-5xl font-mono"> {ReasonReact.string("Things To Do")} </h1>
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
          /*** </ul> **/
          /*** <ul className="list-disc list-inside"> **/
          {switch (self.state.formToggle) {
           | ShowButton =>
             <button type_="button" onClick={_evt => self.send(ToggleForm)} className="btn mt-4">
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
