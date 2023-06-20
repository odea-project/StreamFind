import React, { useState } from "react";
import "./App.css";
import Diagram, { createSchema, useSchema } from "beautiful-react-diagrams";
import FolderIcon from "@mui/icons-material/Folder";
import InsertDriveFileIcon from "@mui/icons-material/InsertDriveFile";
import QueryStatsIcon from "@mui/icons-material/QueryStats";
import Grid from "@mui/material/Unstable_Grid2";
import MenuIcon from "@mui/icons-material/Menu";
import MsData from "./Components/MsData";
import MsAnalysis from "./Components/MsAnalysis";
import MsProcessing from "./Components/MsProcessing";
import { v4 as uuidv4 } from "uuid";

const initialSchema = createSchema({
  nodes: [],
});

const App = () => {
  // create diagrams schema
  const [schema, { onChange, addNode, removeNode }] = useSchema(initialSchema);

  const deleteNodeFromSchema = (id) => {
    const nodeToRemove = schema.nodes.find((node) => node.id === id);
    removeNode(nodeToRemove);
  };

  const addMsData = (e) => {
    const coordinates =
      schema.nodes.length > 0
        ? [
            schema.nodes[schema.nodes.length - 1].coordinates[0] + 100,
            schema.nodes[schema.nodes.length - 1].coordinates[1],
          ]
        : [150, 60];

    const nextNode = {
      id: `nodeMsData-${uuidv4()}`,
      content: `NodeMsData ${schema.nodes.length + 1}`,
      coordinates,
      render: MsData,
      data: { onClick: deleteNodeFromSchema },
      outputs: [
        { id: `portMsAnalysisplay-${Math.random()}` },
        { id: `portMsAnalysisgreen-${Math.random()}` },
        { id: `portMsAnalysisblue-${Math.random()}` },
      ],
    };

    // Create a new schema object by copying the existing nodes and adding the new node
    const newSchema = {
      ...schema,
      nodes: [...schema.nodes, nextNode],
    };

    console.log(e);
    onChange(newSchema); // Update the schema state with the new schema object
  };

  const addMsProcessing = () => {
    const coordinates =
      schema.nodes.length > 0
        ? [
            schema.nodes[schema.nodes.length - 1].coordinates[0] + 100,
            schema.nodes[schema.nodes.length - 1].coordinates[1],
          ]
        : [150, 60];

    const nextNode = {
      id: `nodeMsProcessing-${uuidv4()}`,
      content: `NodeMsProcessing ${schema.nodes.length + 1}`,
      coordinates,
      render: MsProcessing,
      data: { onClick: deleteNodeFromSchema },
      inputs: [
        { id: `portMsPreProcessingout-${Math.random()}` },
        { id: `portMsPreProcessingin-${Math.random()}` },
      ],
    };

    // Create a new schema object by copying the existing nodes and adding the new node
    const newSchema = {
      ...schema,
      nodes: [...schema.nodes, nextNode],
    };

    console.log(nextNode);
    onChange(newSchema); // Update the schema state with the new schema object
  };

  const addMsAnalysis = () => {
    const coordinates =
      schema.nodes.length > 0
        ? [
            schema.nodes[schema.nodes.length - 1].coordinates[0] + 100,
            schema.nodes[schema.nodes.length - 1].coordinates[1],
          ]
        : [150, 60];

    const nextNode = {
      id: `nodeMsAnalysis-${uuidv4()}`,
      content: `NodeMsAnalysis ${schema.nodes.length + 1}`,
      coordinates,
      render: MsAnalysis,
      data: { onClick: deleteNodeFromSchema },
      inputs: [{ id: `portMsData-${Math.random()}` }],
    };

    // Create a new schema object by copying the existing nodes and adding the new node
    const newSchema = {
      ...schema,
      nodes: [...schema.nodes, nextNode],
    };

    console.log(nextNode);
    onChange(newSchema); // Update the schema state with the new schema object
  };

  return (
    <div>
      <Grid container spacing={2}>
        <Grid xs={12}>
          <Diagram
            schema={schema}
            onChange={onChange}
            style={{
              height: "40rem",
              border: "1px solid black",
            }}
          />
        </Grid>
        <div className="demo_box">
          <div className="upper_div">
            <MenuIcon></MenuIcon>
            <h4>Objects</h4>
          </div>
          <FolderIcon
            style={{ fontSize: "6em", color: "orange", cursor: "pointer" }}
            onClick={addMsData}
          ></FolderIcon>
          <h4>MsData</h4>
        </div>
        <div className="demo_box">
          <div className="upper_div">
            <MenuIcon></MenuIcon>
            <h4>Input</h4>
          </div>
          <InsertDriveFileIcon
            style={{ fontSize: "6em", color: "green", cursor: "pointer" }}
            onClick={addMsAnalysis}
          ></InsertDriveFileIcon>
          <h4>MsAnalysis</h4>
        </div>
        <div className="demo_box">
          <div className="upper_div">
            <MenuIcon></MenuIcon>
            <h4>MS-Pre Processing</h4>
          </div>
          <QueryStatsIcon
            style={{ fontSize: "6em", cursor: "pointer" }}
            onClick={addMsProcessing}
          ></QueryStatsIcon>
          <h4>FindFeatures</h4>
        </div>
      </Grid>
    </div>
  );
};

export default App;
