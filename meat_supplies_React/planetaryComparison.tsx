import { csvParse } from "d3";
import React, { useEffect, useRef, useState } from "react";
import { CircularBarTwoCountries } from "@/components/data-vis/planetary-comparison/CircularBarTwoContries.tsx";
import { BarSummary } from "@/components/data-vis/planetary-comparison/barSummary.tsx";
import { PlanetaryLegend } from "@/components/data-vis/planetary-comparison/Legend.tsx";
import { toPng } from "html-to-image";

interface PlanetaryDietCompProps {
  dataContent: string;
  // width: number;
  // height: number;
}

export interface PlanetaryComp {
  food: string;
  grams: number;
  calories: number;
  database: string;
  type: string;
  country: string;
}

function convertCsvToPoint(row: any): PlanetaryComp {
  return {
    food: row.category,
    grams: Number(row.gram),
    calories: Number(row.calorie),
    type: row.type,
    database: row.database,
    country: row.country
  };
}

const DB_FAOSTAT = "FAOSTAT";
const DB_GDD = "GDD";
const gram = "gram";
const calorie = "calorie";

export const PlanetaryComparison = ({ dataContent }: PlanetaryDietCompProps) => {
  const tempData = csvParse(dataContent, convertCsvToPoint);

  const faostatCountries = [
    "Ireland",
    "Denmark",
    "Switzerland",
    "Luxembourg",
    "United States",
    "Australia",
    "Norway",
    "Qatar",
    "Kuwait",
    "Emirates",
    "Taiwan",
    "Saudi Arabia"
  ];

  const [energyFilter, setEnergyFilter] = useState(gram);
  const handleButton = (energyValue: string) => {
    setEnergyFilter(energyValue);
  };

  const [databaseFilter, setDatabaseFilter] = useState(DB_FAOSTAT);
  const handleButtonData = (dbValue: string) => {
    setDatabaseFilter(dbValue);
  };

  const groupedLancet = ["plant", "animal", "oil"].map((name) => ({
    name,
    value: tempData
      .filter((d) => d.country === "Lancet" && d.type === name)
      .map((d) => (energyFilter === gram ? Number(d.grams) || 0 : Number(d.calories) || 0))
      .reduce((a, b) => a + b, 0)
  }));

  const groupedByCountry = faostatCountries.map((country) => {
    const countryData = tempData.filter((d) => d.country === country && d.database === databaseFilter);
    const lancetData = tempData.filter((d) => d.country === "Lancet");
    const summary = ["plant", "animal", "oil"].map((name) => ({
      name,
      value: Math.round(
        countryData
          .filter((d) => d.country !== "Lancet" && d.type === name)
          .map((d) => (energyFilter === gram ? Number(d.grams) || 0 : Number(d.calories) || 0))
          .reduce((a, b) => a + b, 0)
      )
    }));
    return {
      country,
      data: countryData,
      summary,
      lancet: lancetData
    };
  });
  const maxValueAcrossCountries = Math.max(
    ...groupedByCountry.flatMap((country) => country.summary.map((item) => item.value))
  );

  const groupedOnlyLancet = ["Lancet"].map((country) => {
    const lancetData = tempData.filter((d) => d.country === "Lancet");
    const summary = ["plant", "animal", "oil"].map((name) => ({
      name,
      value: Math.round(
        lancetData
          .filter((d) => d.country == "Lancet" && d.type === name)
          .map((d) => (energyFilter === gram ? Number(d.grams) || 0 : Number(d.calories) || 0))
          .reduce((a, b) => a + b, 0)
      )
    }));
    return {
      country,
      data: lancetData,
      summary
    };
  });

  const designWidth = 1200;
  const designHeight = 1200;

  const containerRef = useRef<HTMLDivElement>(null);
  const [measuredWidth, setMeasuredWidth] = useState(0);

  const handleResize = () => {
    setMeasuredWidth((containerRef.current && containerRef.current.getBoundingClientRect().width) ?? 0);
  };

  useEffect(() => {
    handleResize();

    window.addEventListener("resize", handleResize);
    return () => window.removeEventListener("resize", handleResize);
  }, []);

  const respRatio = measuredWidth > designWidth ? 1 : measuredWidth / designWidth;
  const width = designWidth * respRatio;
  const height = designHeight * respRatio;
  const margin = { left: 5, right: 5, top: 20, bottom: 20 };
  const isMobile = width < 450;
  const downloadChart = () => {
    const chartElement = document.getElementById("chart");

    if (chartElement) {
      toPng(chartElement, {
        backgroundColor: "#f1efe6",
        width: 1150,
        height: 1450,
        pixelRatio: 1, //Best practice to keep it low for social media: export your PNG at 2048px max (width or height) so you control the resizing instead of letting Bluesky do it.
        style: {
          margin: "20px",
          background: "#f1efe6"
        }
      }).then((dataUrl) => {
        const link = document.createElement("a");
        link.download = "chart.png";
        link.href = dataUrl;
        link.click();
      });
    }
  };
  return (
    <div id={"chart"} ref={containerRef} className={"w-full flex flex-col relative negating-dataviz-padding"}>
      {/*<h2 className={"not-prose font-bodyVeryBold mt-4 mb-0 text-[#34402f] leading-tight w-full text-sm"}>*/}
      <div className={"relative"}>
        <h2 className={"text-[#34402f] md:text-xl text-lg tracking-tight leading-none "}>
          Food supplies in rich countries contain too many
          <strong className="px-1 ml-1 text-[#b85b54] ">animal-based foods</strong>{" "}
        </h2>
      </div>
      <p className={" text-base-sm mt-2 mb-0 text-[#34402f] leading-tight w-full"}>
        The planetary health diet, developed by an international panel of experts, supports both human and planetary
        health. It places a strong focus on{" "}
        <span className=" bg-[#889181] bg-opacity-30 rounded-full px-1">plant-based foods</span> and limits{" "}
        <span className=" bg-[#b85b54] bg-opacity-30 rounded-full px-1">animal-based products</span> as well as{" "}
        <span className=" bg-[#E1BE6A] bg-opacity-30 rounded-full px-1">fats and oils</span>. However, in many
        high-income countries (based on GDP per capita), food supplies far exceed the recommended intake targets for
        meat and dairy.
      </p>
      {isMobile && (
        <div>
          <p className={" text-base mt-4 mb-4 text-[#34402f] leading-tight w-full"}>
            📖︎ <strong className={"text-[#34402f]"}>Radial charts</strong> show daily availability of 11 food groups
            per person (based on food supplied to shops) compared to planetary health targets. Hover for details.{" "}
            <strong className={"text-[#34402f]"}>Bars</strong> below show total supply from plants, animals, and oils.
            Use toggle to switch between grams and calories.
          </p>
        </div>
      )}
      <div className="flex flex-row justify-stretch items-center">
        <div id="buttonsWeight" className={""}>
          <div className="font-bodyBold mr-8 flex flex-row w-[30%]">
            <button
              onClick={() => handleButton(gram)}
              className={`${energyFilter === gram ? "bg-[#34402f] text-site-background" : "shadow-sm shadow-[#3B4D61]"} md:text-base text-sm px-2 py-1 text-center me-0 mb-0 rounded-l-lg`}
            >
              Grams
            </button>
            <button
              onClick={() => handleButton(calorie)}
              className={`${energyFilter === calorie ? "bg-[#34402f] text-site-background" : "shadow-sm shadow-[#3B4D61]"} md:text-base text-sm px-2 py-1 text-center me-0 mb-0 rounded-r-lg`}
            >
              Calories
            </button>
          </div>
        </div>
        {!isMobile && (
          <div>
            <p className={" text-base mt-4 mb-4 text-[#34402f] leading-tight w-full"}>
              📖︎ <strong className={"text-[#34402f]"}>Radial charts</strong> show daily availability of 11 food groups
              per person (based on food supplied to shops) compared to planetary health targets. Hover for details.{" "}
              <strong className={"text-[#34402f]"}>Bars</strong> below show total supply from plants, animals, and oils.
              Use toggle to switch between grams and calories.
            </p>
          </div>
        )}
      </div>
      <div
        id="mainChart"
        className={"relative grid grid-cols-1 md:grid-cols-3 lg:grid-cols-4 gap-4 md:w-[70%] w-full h-full mt-4"}
      >
        <div className={"w-full md:w-[300px] block md:absolute right-[-330px] top-[0px]"}>
          <PlanetaryLegend
            legendData={groupedOnlyLancet}
            groupedLegendData={groupedLancet}
            energyFilter={energyFilter}
            isMobile={isMobile}
          />
          {!isMobile && (
            <div id={"caption"}>
              <div className={" flex justify-start md:mt-4 mt-4 leading-none w-[full]"}>
                <p className={"my-0"}>
                  <span className="mr-1 font-body md:text-base text-xs text-gray-600">Planetary diet:</span>
                  <span className={"font-body md:text-base text-xs text-gray-600 inline"}>
                    <a
                      href="https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(18)31788-4/abstract"
                      target="_blank"
                      className="underline underline-offset-2 decoration-[#9294a8] bg-[#5146b8] bg-opacity-5 hover:bg-opacity-100 text-gray-600 hover:text-almostWhite mr-1"
                    >
                      Food in the Anthropocene: the EAT–Lancet Commission on healthy diets from sustainable food systems
                    </a>
                  </span>
                </p>
              </div>
              <div className={" flex justify-start md:mt-2 mt-2 leading-none w-[full]"}>
                <p className={"my-0"}>
                  <span className="mr-1 font-body md:text-base text-xs text-gray-600">Supply data:</span>
                  <span className={"font-body md:text-base text-xs text-gray-600 inline"}>
                    <a
                      href="https://www.fao.org/faostat/en/#data/SUA"
                      target="_blank"
                      className="underline underline-offset-2 decoration-[#9294a8] bg-[#5146b8] bg-opacity-5 hover:bg-opacity-100 text-gray-600 hover:text-almostWhite mr-1"
                    >
                      Food and Agriculture Organization of the United Nations (2022).
                    </a>{" "}
                    It estimates food available for human consumption at the retail level per person per day (not actual
                    intake)
                  </span>
                </p>
              </div>
              <div className={"w-full flex justify-start md:mt-0 mt-2 "}>
                <p className={"my-0"}>
                  <span className=" font-body md:text-base text-xs text-gray-600 ">
                    Design:
                    <span className={" ml-1 inline font-body md:text-base text-xs text-gray-600"}>
                      <a
                        href="https://bsky.app/profile/lumipie.com"
                        target="_blank"
                        className="underline underline-offset-2 decoration-[#9294a8] bg-[#5146b8] bg-opacity-5 hover:bg-opacity-100 text-gray-600 hover:text-almostWhite mr-1"
                      >
                        Elena Shekhova
                      </a>
                    </span>
                  </span>
                </p>
              </div>
              <div className={"w-full flex justify-start md:mt-0 mt-2 "}>
                <p className={"my-0"}>
                  <span className=" font-body md:text-base text-xs text-gray-600 ">
                    Data preparation:
                    <span className={" ml-1 inline font-body md:text-base text-xs text-gray-600"}>
                      <a
                        href="https://github.com/ElenaShekhova/Data-preparation/blob/a591a8cafae77e087cab188ec676c8ad33b9523f/SupplyComparison20250912.rmd"
                        target="_blank"
                        className="underline underline-offset-2 decoration-[#9294a8] bg-[#5146b8] bg-opacity-5 hover:bg-opacity-100 text-gray-600 hover:text-almostWhite mr-1"
                      >
                        R code
                      </a>
                    </span>
                  </span>
                </p>
              </div>
            </div>
          )}
        </div>
        {groupedByCountry.map((group) => (
          <div key={group.country} className="flex flex-col items-center bg-[#faf7ed]">
            <h3 className="text-base-sm font-bodyVeryBold text-blackSt mt-2">{group.country}</h3>
            <div className="overflow-visible w-full ">
              <CircularBarTwoCountries
                data={group.data}
                isMobile={isMobile}
                energyFilter={energyFilter}
                lancet={group.lancet}
              />
            </div>
            <div className={"pl-4 overflow-visible w-full"}>
              <BarSummary
                isMobile={isMobile}
                dataCountry={group.summary}
                Lancet={groupedLancet}
                energyFilter={energyFilter}
                maxDomainValue={maxValueAcrossCountries}
              />
            </div>
          </div>
        ))}
        <div>
          {isMobile && (
            <div id={"captionMobile"}>
              <div className={" flex justify-start md:mt-4 mt-4 leading-tight w-[full]"}>
                <p className={"my-0"}>
                  <span className="mr-1 font-body md:text-base text-gray-600">Planetary diet:</span>
                  <span className={"font-body md:text-base text-gray-600 inline"}>
                    <a
                      href="https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(18)31788-4/abstract"
                      target="_blank"
                      className="underline underline-offset-2 decoration-[#9294a8] bg-[#5146b8] bg-opacity-5 hover:bg-opacity-100 text-gray-600 hover:text-almostWhite mr-1"
                    >
                      Food in the Anthropocene: the EAT–Lancet Commission on healthy diets from sustainable food systems
                    </a>
                  </span>
                </p>
              </div>
              <div className={" flex justify-start md:mt-2 mt-2 leading-tight w-[full]"}>
                <p className={"my-0"}>
                  <span className="mr-1 font-body md:text-base text-gray-600">Supply data:</span>
                  <span className={"font-body md:text-base text-gray-600 inline"}>
                    <a
                      href="https://www.fao.org/faostat/en/#data/SUA"
                      target="_blank"
                      className="underline underline-offset-2 decoration-[#9294a8] bg-[#5146b8] bg-opacity-5 hover:bg-opacity-100 text-gray-600 hover:text-almostWhite mr-1"
                    >
                      Food and Agriculture Organization of the United Nations (2022).
                    </a>{" "}
                    It estimates food available for human consumption at the retail level per person per day (not actual
                    intake)
                  </span>
                </p>
              </div>
              <div className={"w-full flex justify-start md:mt-0 mt-2 leading-tight "}>
                <p className={"my-0"}>
                  <span className=" font-body md:text-base text-gray-600 ">
                    Design:
                    <span className={" ml-1 inline font-body md:text-base text-gray-600"}>
                      <a
                        href="https://bsky.app/profile/lumipie.com"
                        target="_blank"
                        className="underline underline-offset-2 decoration-[#9294a8] bg-[#5146b8] bg-opacity-5 hover:bg-opacity-100 text-gray-600 hover:text-almostWhite mr-1"
                      >
                        Elena Shekhova
                      </a>
                    </span>
                  </span>
                </p>
              </div>
              <div className={"w-full flex justify-start md:mt-0 mt-2 "}>
                <p className={"my-0"}>
                  <span className=" font-body md:text-base text-gray-600 ">
                    Data preparation:
                    <span className={" ml-1 inline font-body md:text-base text-gray-600"}>
                      <a
                        href="https://github.com/ElenaShekhova/Data-preparation/blob/a591a8cafae77e087cab188ec676c8ad33b9523f/SupplyComparison20250912.rmd"
                        target="_blank"
                        className="underline underline-offset-2 decoration-[#9294a8] bg-[#5146b8] bg-opacity-5 hover:bg-opacity-100 text-gray-600 hover:text-almostWhite mr-1"
                      >
                        R code
                      </a>
                    </span>
                  </span>
                </p>
              </div>
            </div>
          )}
        </div>
      </div>
      {databaseFilter === DB_GDD && (
        <div className="w-full font-body md:text-sm text-xs">* Data for Added sugars and Added fats is missing</div>
      )}

      {/*<button className={"mt-16"} onClick={downloadChart}>*/}
      {/*  Download Chart*/}
      {/*</button>*/}
    </div>
  );
};
