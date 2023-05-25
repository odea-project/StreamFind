import React, { useState } from "react";
import axios from "axios";

const MyComponent = () => {
  const [data, setData] = useState({
    name: "",
    email: "",
  });

  const handleChange = (e) => {
    setData({ ...data, [e.target.name]: e.target.value });
  };

  const handleSubmit = (e) => {
    e.preventDefault();
    axios
      .post("http://127.0.0.1:6058/api", data)
      .then((response) => {
        console.log(response.data);
        // Do something with the response data
      })
      .catch((error) => {
        console.error(error);
        // Handle error
      });
  };

  return (
    <form onSubmit={handleSubmit}>
      <input
        type="text"
        name="name"
        value={data.name}
        onChange={handleChange}
      />
      <input
        type="email"
        name="email"
        value={data.email}
        onChange={handleChange}
      />
      <button type="submit">Submit</button>
    </form>
  );
};

export default MyComponent;
