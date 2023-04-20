import {MsData} from "./MsData";
import {MsAnalysis} from "./MsAnalysis";
import '../styles/Objects.css';


export const Objects = () => {
  return (
      <>
          <div className="container">
              <div className="left">
                <MsAnalysis className="icon" /> <br />
              </div>
              <div className="right">
                <MsData className="icon"  />
              </div>
          </div>
      </>
  )
}
